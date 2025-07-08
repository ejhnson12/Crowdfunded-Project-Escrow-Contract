(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-goal-not-reached (err u104))
(define-constant err-deadline-passed (err u105))
(define-constant err-deadline-not-passed (err u106))
(define-constant err-already-claimed (err u107))
(define-constant err-already-refunded (err u108))
(define-constant err-invalid-deadline (err u109))
(define-constant err-project-not-active (err u110))
(define-constant err-insufficient-funds (err u111))

(define-constant err-milestone-not-found (err u200))
(define-constant err-milestone-already-claimed (err u201))
(define-constant err-milestone-invalid-percentage (err u202))
(define-constant err-milestone-conditions-not-met (err u203))
(define-constant err-milestone-limit-exceeded (err u204))

(define-data-var next-project-id uint u1)

(define-map projects
  { project-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-goal: uint,
    deadline: uint,
    total-raised: uint,
    claimed: bool,
    refunded: bool,
    active: bool
  }
)

(define-map contributions
  { project-id: uint, contributor: principal }
  { amount: uint }
)

(define-map project-contributors
  { project-id: uint }
  { contributors: (list 1000 principal) }
)

(define-map contributor-projects
  { contributor: principal }
  { projects: (list 100 uint) }
)

(define-read-only (get-project (project-id uint))
  (ok (map-get? projects { project-id: project-id }))
)

(define-read-only (get-contribution (project-id uint) (contributor principal))
  (ok (map-get? contributions { project-id: project-id, contributor: contributor }))
)

(define-read-only (get-current-block-height)
  (ok stacks-block-height)
)

(define-read-only (get-next-project-id)
  (ok (var-get next-project-id))
)

(define-read-only (get-project-contributors (project-id uint))
  (ok (default-to (list) (get contributors (map-get? project-contributors { project-id: project-id }))))
)

(define-read-only (get-contributor-projects (contributor principal))
  (ok (default-to (list) (get projects (map-get? contributor-projects { contributor: contributor }))))
)

(define-read-only (is-deadline-passed (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project-data
    (ok (>= stacks-block-height (get deadline project-data)))
    (err err-not-found)
  )
)

(define-read-only (is-goal-reached (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project-data
    (ok (>= (get total-raised project-data) (get funding-goal project-data)))
    (err err-not-found)
  )
)

(define-read-only (can-claim-funds (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project-data
    (let
      (
        (goal-reached (>= (get total-raised project-data) (get funding-goal project-data)))
        (deadline-passed (>= stacks-block-height (get deadline project-data)))
        (not-claimed (not (get claimed project-data)))
        (is-creator (is-eq tx-sender (get creator project-data)))
      )
      (ok (and goal-reached deadline-passed not-claimed is-creator))
    )
    (err err-not-found)
  )
)

(define-read-only (can-get-refund (project-id uint) (contributor principal))
  (match (map-get? projects { project-id: project-id })
    project-data
    (let
      (
        (goal-not-reached (< (get total-raised project-data) (get funding-goal project-data)))
        (deadline-passed (>= stacks-block-height (get deadline project-data)))
        (not-refunded (not (get refunded project-data)))
        (has-contribution (is-some (map-get? contributions { project-id: project-id, contributor: contributor })))
      )
      (ok (and goal-not-reached deadline-passed not-refunded has-contribution))
    )
    (err err-not-found)
  )
)

(define-private (add-contributor-to-project (project-id uint) (contributor principal))
  (let
    (
      (current-contributors (default-to (list) (get contributors (map-get? project-contributors { project-id: project-id }))))
      (updated-contributors (unwrap! (as-max-len? (append current-contributors contributor) u1000) (err err-insufficient-funds)))
    )
    (map-set project-contributors
      { project-id: project-id }
      { contributors: updated-contributors }
    )
    (ok true)
  )
)

(define-private (add-project-to-contributor (project-id uint) (contributor principal))
  (let
    (
      (current-projects (default-to (list) (get projects (map-get? contributor-projects { contributor: contributor }))))
      (updated-projects (unwrap! (as-max-len? (append current-projects project-id) u100) (err err-insufficient-funds)))
    )
    (map-set contributor-projects
      { contributor: contributor }
      { projects: updated-projects }
    )
    (ok true)
  )
)

(define-public (create-project (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint) (deadline uint))
  (let
    (
      (project-id (var-get next-project-id))
      (current-height stacks-block-height)
    )
    (asserts! (> funding-goal u0) err-invalid-amount)
    (asserts! (> deadline current-height) err-invalid-deadline)
    (map-set projects
      { project-id: project-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        deadline: deadline,
        total-raised: u0,
        claimed: false,
        refunded: false,
        active: true
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (contribute (project-id uint) (amount uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (current-contribution (default-to u0 (get amount (map-get? contributions { project-id: project-id, contributor: tx-sender }))))
      (new-total-contribution (+ current-contribution amount))
      (new-project-total (+ (get total-raised project-data) amount))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get active project-data) err-project-not-active)
    (asserts! (< stacks-block-height (get deadline project-data)) err-deadline-passed)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set contributions
      { project-id: project-id, contributor: tx-sender }
      { amount: new-total-contribution }
    )
    (map-set projects
      { project-id: project-id }
      (merge project-data { total-raised: new-project-total })
    )
    (if (is-eq current-contribution u0)
      (begin
        (unwrap! (add-contributor-to-project project-id tx-sender) err-insufficient-funds)
        (unwrap! (add-project-to-contributor project-id tx-sender) err-insufficient-funds)    )
      true
    )
    (ok new-total-contribution)
  )
)

(define-public (claim-funds (project-id uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator project-data)) err-unauthorized)
    (asserts! (>= (get total-raised project-data) (get funding-goal project-data)) err-goal-not-reached)
    (asserts! (>= stacks-block-height (get deadline project-data)) err-deadline-not-passed)
    (asserts! (not (get claimed project-data)) err-already-claimed)
    (try! (as-contract (stx-transfer? (get total-raised project-data) tx-sender (get creator project-data))))
    (map-set projects
      { project-id: project-id }
      (merge project-data { claimed: true, active: false })
    )
    (ok (get total-raised project-data))
  )
)

(define-public (refund (project-id uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (contribution-data (unwrap! (map-get? contributions { project-id: project-id, contributor: tx-sender }) err-not-found))
      (refund-amount (get amount contribution-data))
    )
    (asserts! (< (get total-raised project-data) (get funding-goal project-data)) err-goal-not-reached)
    (asserts! (>= stacks-block-height (get deadline project-data)) err-deadline-not-passed)
    (asserts! (> refund-amount u0) err-invalid-amount)
    (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
    (map-delete contributions { project-id: project-id, contributor: tx-sender })
    (ok refund-amount)
  )
)

(define-public (cancel-project (project-id uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator project-data)) err-unauthorized)
    (asserts! (get active project-data) err-project-not-active)
    (asserts! (is-eq (get total-raised project-data) u0) err-invalid-amount)
    (map-set projects
      { project-id: project-id }
      (merge project-data { active: false })
    )
    (ok true)
  )
)

(define-public (mark-project-for-refund (project-id uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    )
    (asserts! (< (get total-raised project-data) (get funding-goal project-data)) err-goal-not-reached)
    (asserts! (>= stacks-block-height (get deadline project-data)) err-deadline-not-passed)
    (asserts! (not (get refunded project-data)) err-already-refunded)
    (map-set projects
      { project-id: project-id }
      (merge project-data { refunded: true, active: false })
    )
    (ok true)
  )
)

(define-map project-milestones
  { project-id: uint, milestone-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    percentage: uint,
    unlock-condition: (string-ascii 200),
    claimed: bool,
    claim-deadline: uint
  }
)

(define-map milestone-count
  { project-id: uint }
  { count: uint }
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (ok (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }))
)

(define-read-only (get-milestone-count (project-id uint))
  (ok (default-to u0 (get count (map-get? milestone-count { project-id: project-id }))))
)

(define-read-only (calculate-milestone-amount (project-id uint) (milestone-id uint))
  (match (map-get? projects { project-id: project-id })
    project-data
    (match (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
      milestone-data
      (ok (/ (* (get total-raised project-data) (get percentage milestone-data)) u100))
      (err err-milestone-not-found)
    )
    (err err-not-found)
  )
)

(define-public (create-milestone (project-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (percentage uint) (unlock-condition (string-ascii 200)) (claim-deadline uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (current-count (default-to u0 (get count (map-get? milestone-count { project-id: project-id }))))
      (new-milestone-id (+ current-count u1))
    )
    (asserts! (is-eq tx-sender (get creator project-data)) err-unauthorized)
    (asserts! (and (> percentage u0) (<= percentage u100)) err-milestone-invalid-percentage)
    (asserts! (< current-count u10) err-milestone-limit-exceeded)
    (asserts! (> claim-deadline stacks-block-height) err-invalid-deadline)
    (map-set project-milestones
      { project-id: project-id, milestone-id: new-milestone-id }
      {
        title: title,
        description: description,
        percentage: percentage,
        unlock-condition: unlock-condition,
        claimed: false,
        claim-deadline: claim-deadline
      }
    )
    (map-set milestone-count
      { project-id: project-id }
      { count: new-milestone-id }
    )
    (ok new-milestone-id)
  )
)

(define-public (claim-milestone (project-id uint) (milestone-id uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (milestone-data (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) err-milestone-not-found))
      (milestone-amount (unwrap! (calculate-milestone-amount project-id milestone-id) err-milestone-not-found))
    )
    (asserts! (is-eq tx-sender (get creator project-data)) err-unauthorized)
    (asserts! (not (get claimed milestone-data)) err-milestone-already-claimed)
    (asserts! (< stacks-block-height (get claim-deadline milestone-data)) err-deadline-passed)
    (asserts! (>= (get total-raised project-data) (get funding-goal project-data)) err-goal-not-reached)
    (try! (as-contract (stx-transfer? milestone-amount tx-sender (get creator project-data))))
    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone-data { claimed: true })
    )
    (ok milestone-amount)
  )
)
