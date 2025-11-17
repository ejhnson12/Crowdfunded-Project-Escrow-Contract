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

(define-constant err-update-limit-exceeded (err u300))
(define-constant err-empty-update (err u301))

(define-constant err-rating-unauthorized (err u400))
(define-constant err-rating-invalid-score (err u401))
(define-constant err-rating-already-submitted (err u402))
(define-constant err-rating-project-not-completed (err u403))

(define-constant err-emergency-already-initiated (err u500))
(define-constant err-emergency-not-active (err u501))
(define-constant err-emergency-threshold-not-met (err u502))
(define-constant err-emergency-already-voted (err u503))
(define-constant err-emergency-cooldown-active (err u504))

(define-constant err-schedule-exists (err u600))
(define-constant err-schedule-not-found (err u601))
(define-constant err-schedule-invalid-config (err u602))
(define-constant err-schedule-not-unlocked (err u603))
(define-constant err-schedule-already-claimed (err u604))
(define-constant err-schedule-exceeds-total (err u605))

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


(define-map project-updates
  { project-id: uint, update-id: uint }
  {
    title: (string-ascii 100),
    content: (string-ascii 1000),
    timestamp: uint,
    block-height: uint
  }
)

(define-map update-count
  { project-id: uint }
  { count: uint }
)

(define-read-only (get-update (project-id uint) (update-id uint))
  (ok (map-get? project-updates { project-id: project-id, update-id: update-id }))
)

(define-read-only (get-update-count (project-id uint))
  (ok (default-to u0 (get count (map-get? update-count { project-id: project-id }))))
)

(define-read-only (get-latest-update (project-id uint))
  (let
    (
      (total-updates (default-to u0 (get count (map-get? update-count { project-id: project-id }))))
    )
    (if (> total-updates u0)
      (ok (map-get? project-updates { project-id: project-id, update-id: total-updates }))
      (ok none)
    )
  )
)

(define-public (post-update (project-id uint) (title (string-ascii 100)) (content (string-ascii 1000)))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (current-count (default-to u0 (get count (map-get? update-count { project-id: project-id }))))
      (new-update-id (+ current-count u1))
    )
    (asserts! (is-eq tx-sender (get creator project-data)) err-unauthorized)
    (asserts! (get active project-data) err-project-not-active)
    (asserts! (> (len title) u0) err-empty-update)
    (asserts! (> (len content) u0) err-empty-update)
    (asserts! (< current-count u50) err-update-limit-exceeded)
    (map-set project-updates
      { project-id: project-id, update-id: new-update-id }
      {
        title: title,
        content: content,
        timestamp: stacks-block-height,
        block-height: stacks-block-height
      }
    )
    (map-set update-count
      { project-id: project-id }
      { count: new-update-id }
    )
    (ok new-update-id)
  )
)

(define-map project-ratings
  { project-id: uint, contributor: principal }
  { score: uint, comment: (string-ascii 200) }
)

(define-map project-rating-summary
  { project-id: uint }
  { 
    total-score: uint,
    rating-count: uint,
    average-score: uint
  }
)

(define-map creator-reputation
  { creator: principal }
  {
    total-projects-rated: uint,
    cumulative-score: uint,
    average-reputation: uint
  }
)

(define-read-only (get-project-rating (project-id uint) (contributor principal))
  (ok (map-get? project-ratings { project-id: project-id, contributor: contributor }))
)

(define-read-only (get-project-rating-summary (project-id uint))
  (ok (map-get? project-rating-summary { project-id: project-id }))
)

(define-read-only (get-creator-reputation (creator principal))
  (ok (map-get? creator-reputation { creator: creator }))
)

(define-public (rate-project (project-id uint) (score uint) (comment (string-ascii 200)))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (contribution-data (unwrap! (map-get? contributions { project-id: project-id, contributor: tx-sender }) err-rating-unauthorized))
      (current-summary (default-to { total-score: u0, rating-count: u0, average-score: u0 } (map-get? project-rating-summary { project-id: project-id })))
      (creator (get creator project-data))
      (current-reputation (default-to { total-projects-rated: u0, cumulative-score: u0, average-reputation: u0 } (map-get? creator-reputation { creator: creator })))
    )
    (asserts! (and (>= score u1) (<= score u5)) err-rating-invalid-score)
    (asserts! (get claimed project-data) err-rating-project-not-completed)
    (asserts! (is-none (map-get? project-ratings { project-id: project-id, contributor: tx-sender })) err-rating-already-submitted)
    
    (map-set project-ratings
      { project-id: project-id, contributor: tx-sender }
      { score: score, comment: comment }
    )
    
    (let
      (
        (new-total-score (+ (get total-score current-summary) score))
        (new-rating-count (+ (get rating-count current-summary) u1))
        (new-average (/ new-total-score new-rating-count))
      )
      (map-set project-rating-summary
        { project-id: project-id }
        { 
          total-score: new-total-score,
          rating-count: new-rating-count,
          average-score: new-average
        }
      )
      
      (let
        (
          (new-projects-rated (+ (get total-projects-rated current-reputation) u1))
          (new-cumulative (+ (get cumulative-score current-reputation) score))
          (new-reputation-avg (/ new-cumulative new-projects-rated))
        )
        (map-set creator-reputation
          { creator: creator }
          {
            total-projects-rated: new-projects-rated,
            cumulative-score: new-cumulative,
            average-reputation: new-reputation-avg
          }
        )
      )
    )
    
    (ok score)
  )
)

(define-map emergency-withdrawal-status
  { project-id: uint }
  {
    active: bool,
    vote-count: uint,
    total-votes-required: uint,
    initiation-height: uint,
    resolution-deadline: uint
  }
)

(define-map emergency-votes
  { project-id: uint, voter: principal }
  { voted: bool, vote-height: uint }
)

(define-read-only (get-emergency-status (project-id uint))
  (ok (map-get? emergency-withdrawal-status { project-id: project-id }))
)

(define-read-only (has-voted-emergency (project-id uint) (voter principal))
  (ok (default-to false (get voted (map-get? emergency-votes { project-id: project-id, voter: voter }))))
)

(define-read-only (calculate-proportional-share (project-id uint) (contributor principal))
  (match (map-get? projects { project-id: project-id })
    project-data
    (match (map-get? contributions { project-id: project-id, contributor: contributor })
      contribution-data
      (ok (/ (* (get amount contribution-data) u10000) (get total-raised project-data)))
      (err err-not-found)
    )
    (err err-not-found)
  )
)

(define-public (initiate-emergency-withdrawal (project-id uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (contribution-data (unwrap! (map-get? contributions { project-id: project-id, contributor: tx-sender }) err-unauthorized))
      (contributors-list (default-to (list) (get contributors (map-get? project-contributors { project-id: project-id }))))
      (total-contributors (len contributors-list))
      (votes-needed (/ (* total-contributors u60) u100))
    )
    (asserts! (>= (get total-raised project-data) (get funding-goal project-data)) err-goal-not-reached)
    (asserts! (>= stacks-block-height (+ (get deadline project-data) u144)) err-emergency-cooldown-active)
    (asserts! (not (get claimed project-data)) err-already-claimed)
    (asserts! (is-none (map-get? emergency-withdrawal-status { project-id: project-id })) err-emergency-already-initiated)
    (map-set emergency-withdrawal-status
      { project-id: project-id }
      {
        active: true,
        vote-count: u1,
        total-votes-required: votes-needed,
        initiation-height: stacks-block-height,
        resolution-deadline: (+ stacks-block-height u1008)
      }
    )
    (map-set emergency-votes
      { project-id: project-id, voter: tx-sender }
      { voted: true, vote-height: stacks-block-height }
    )
    (ok true)
  )
)

(define-public (vote-emergency-withdrawal (project-id uint))
  (let
    (
      (emergency-data (unwrap! (map-get? emergency-withdrawal-status { project-id: project-id }) err-emergency-not-active))
      (contribution-data (unwrap! (map-get? contributions { project-id: project-id, contributor: tx-sender }) err-unauthorized))
      (has-voted (default-to false (get voted (map-get? emergency-votes { project-id: project-id, voter: tx-sender }))))
      (new-vote-count (+ (get vote-count emergency-data) u1))
    )
    (asserts! (get active emergency-data) err-emergency-not-active)
    (asserts! (< stacks-block-height (get resolution-deadline emergency-data)) err-deadline-passed)
    (asserts! (not has-voted) err-emergency-already-voted)
    (map-set emergency-votes
      { project-id: project-id, voter: tx-sender }
      { voted: true, vote-height: stacks-block-height }
    )
    (map-set emergency-withdrawal-status
      { project-id: project-id }
      (merge emergency-data { vote-count: new-vote-count })
    )
    (ok new-vote-count)
  )
)

(define-public (execute-emergency-withdrawal (project-id uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (emergency-data (unwrap! (map-get? emergency-withdrawal-status { project-id: project-id }) err-emergency-not-active))
      (contribution-data (unwrap! (map-get? contributions { project-id: project-id, contributor: tx-sender }) err-unauthorized))
      (proportional-basis (unwrap! (calculate-proportional-share project-id tx-sender) err-not-found))
      (withdrawal-amount (/ (* (get total-raised project-data) proportional-basis) u10000))
    )
    (asserts! (get active emergency-data) err-emergency-not-active)
    (asserts! (>= (get vote-count emergency-data) (get total-votes-required emergency-data)) err-emergency-threshold-not-met)
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))
    (map-delete contributions { project-id: project-id, contributor: tx-sender })
    (ok withdrawal-amount)
  )
)


(define-map withdrawal-schedules
  { project-id: uint }
  {
    intervals: uint,
    interval-duration: uint,
    start-block: uint,
    amount-per-interval: uint,
    total-scheduled: uint
  }
)

(define-map schedule-claims
  { project-id: uint }
  { intervals-claimed: uint, total-claimed: uint }
)

(define-read-only (get-withdrawal-schedule (project-id uint))
  (ok (map-get? withdrawal-schedules { project-id: project-id }))
)

(define-read-only (get-schedule-claims (project-id uint))
  (ok (map-get? schedule-claims { project-id: project-id }))
)

(define-read-only (calculate-unlocked-intervals (project-id uint))
  (match (map-get? withdrawal-schedules { project-id: project-id })
    schedule
    (let
      (
        (blocks-elapsed (- stacks-block-height (get start-block schedule)))
        (unlocked (/ blocks-elapsed (get interval-duration schedule)))
      )
      (ok (if (> unlocked (get intervals schedule))
            (get intervals schedule)
            unlocked))
    )
    (err err-schedule-not-found)
  )
)

(define-read-only (calculate-claimable-amount (project-id uint))
  (match (map-get? withdrawal-schedules { project-id: project-id })
    schedule
    (let
      (
        (unlocked-intervals (unwrap! (calculate-unlocked-intervals project-id) err-schedule-not-found))
        (claims (default-to { intervals-claimed: u0, total-claimed: u0 } (map-get? schedule-claims { project-id: project-id })))
        (claimable-intervals (- unlocked-intervals (get intervals-claimed claims)))
      )
      (ok (* claimable-intervals (get amount-per-interval schedule)))
    )
    (ok u0)
  )
)

(define-public (create-withdrawal-schedule (project-id uint) (intervals uint) (interval-duration uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (amount-per-interval (/ (get total-raised project-data) intervals))
      (total-scheduled (* amount-per-interval intervals))
    )
    (asserts! (is-eq tx-sender (get creator project-data)) err-unauthorized)
    (asserts! (>= (get total-raised project-data) (get funding-goal project-data)) err-goal-not-reached)
    (asserts! (>= stacks-block-height (get deadline project-data)) err-deadline-not-passed)
    (asserts! (is-none (map-get? withdrawal-schedules { project-id: project-id })) err-schedule-exists)
    (asserts! (and (> intervals u0) (<= intervals u24)) err-schedule-invalid-config)
    (asserts! (> interval-duration u0) err-schedule-invalid-config)
    (map-set withdrawal-schedules
      { project-id: project-id }
      {
        intervals: intervals,
        interval-duration: interval-duration,
        start-block: stacks-block-height,
        amount-per-interval: amount-per-interval,
        total-scheduled: total-scheduled
      }
    )
    (ok true)
  )
)

(define-public (claim-scheduled-withdrawal (project-id uint))
  (let
    (
      (project-data (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
      (schedule (unwrap! (map-get? withdrawal-schedules { project-id: project-id }) err-schedule-not-found))
      (claimable (unwrap! (calculate-claimable-amount project-id) err-schedule-not-found))
      (claims (default-to { intervals-claimed: u0, total-claimed: u0 } (map-get? schedule-claims { project-id: project-id })))
      (unlocked-intervals (unwrap! (calculate-unlocked-intervals project-id) err-schedule-not-found))
    )
    (asserts! (is-eq tx-sender (get creator project-data)) err-unauthorized)
    (asserts! (> claimable u0) err-schedule-not-unlocked)
    (try! (as-contract (stx-transfer? claimable tx-sender (get creator project-data))))
    (map-set schedule-claims
      { project-id: project-id }
      { 
        intervals-claimed: unlocked-intervals,
        total-claimed: (+ (get total-claimed claims) claimable)
      }
    )
    (ok claimable)
  )
)