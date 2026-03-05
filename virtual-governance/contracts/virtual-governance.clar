;; Virtual Governance DAO
;; Clarity Version 2, Epoch 2.1
;; Decentralized governance with reputation-weighted quadratic voting

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-MEMBER (err u101))
(define-constant ERR-NOT-MEMBER (err u102))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-PROPOSAL-ACTIVE (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-INVALID-AMOUNT (err u107))
(define-constant ERR-PROPOSAL-EXECUTED (err u108))
(define-constant ERR-QUORUM-NOT-MET (err u109))
(define-constant ERR-VOTE-NOT-PASSED (err u110))

;; Reputation decay rate: applied per epoch (roughly per day in blocks)
;; 1 epoch = 144 blocks (~1 day at 10 min/block)
(define-constant BLOCKS-PER-EPOCH u144)

;; Minimum reputation to submit a proposal
(define-constant MIN-REPUTATION-TO-PROPOSE u100)

;; Quorum threshold: at least 20% of total reputation must participate
(define-constant QUORUM-PERCENT u20)

;; Proposal voting duration in blocks (~7 days)
(define-constant VOTING-PERIOD u1008)

;; Decay factor: reputation decays by 1% per epoch of inactivity
;; Represented as basis points: 9900 = 99% retained per epoch
(define-constant DECAY-BASIS-POINTS u9900)

;; Max votes a member can cast on a single proposal (quadratic cost)
(define-constant MAX-VOTES-PER-PROPOSAL u10)

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; Global counters
(define-data-var proposal-count uint u0)
(define-data-var total-reputation uint u0)

;; Member registry
;; reputation       - current reputation score
;; last-active      - block height of last participation
;; proposals-made   - total proposals submitted
;; votes-cast       - total votes cast
;; correct-outcomes - proposals voted on that passed and succeeded
(define-map members
  { address: principal }
  {
    reputation: uint,
    last-active: uint,
    proposals-made: uint,
    votes-cast: uint,
    correct-outcomes: uint,
    domain-tags: (list 5 (string-ascii 32))
  }
)

;; Proposal registry
;; status: 0=active, 1=passed, 2=rejected, 3=executed, 4=cancelled
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 64),
    description: (string-ascii 256),
    domain: (string-ascii 32),
    created-at: uint,
    voting-ends-at: uint,
    yes-votes: uint,
    no-votes: uint,
    yes-reputation: uint,
    no-reputation: uint,
    status: uint,
    executed: bool
  }
)

;; Track votes per member per proposal
;; votes-used - number of vote units spent (quadratic: cost = votes^2 reputation)
(define-map member-votes
  { proposal-id: uint, voter: principal }
  {
    direction: bool,
    votes-used: uint,
    reputation-at-vote: uint
  }
)

;; Mediation pool: tracks active mediators for conflict resolution
(define-map mediators
  { address: principal }
  { active: bool, cases-resolved: uint }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Compute quadratic vote cost: cost = votes * votes (reputation units)
(define-private (quadratic-cost (votes uint))
  (* votes votes)
)

;; Apply temporal decay to a reputation score.
;; For each elapsed epoch since last-active, reduce by decay factor.
;; Simplified: decay = reputation * (9900^epochs / 10000^epochs)
;; We approximate with a single-step decay for simplicity.
;; Clarity has no built-in min, so we cap epochs at 10 manually.
(define-private (apply-decay (rep uint) (last-active uint))
  (let (
    (elapsed (- block-height last-active))
    (epochs (/ elapsed BLOCKS-PER-EPOCH))
    (capped-epochs (if (> epochs u10) u10 epochs))
  )
    ;; Each epoch: rep = rep * 9900 / 10000
    ;; Beyond 10 epochs, decay is capped at ~90% total reduction
    (if (> epochs u0)
      (/ (* rep (- u10000 (* capped-epochs u100))) u10000)
      rep
    )
  )
)

;; Get effective (decay-adjusted) reputation for a member
(define-private (effective-reputation (address principal))
  (match (map-get? members { address: address })
    member (apply-decay (get reputation member) (get last-active member))
    u0
  )
)

;; Safely get a proposal or return none
(define-private (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Check if a principal is a registered member
(define-private (is-member (address principal))
  (is-some (map-get? members { address: address }))
)

;; Increment a member's vote count and update last-active
(define-private (record-activity (address principal))
  (match (map-get? members { address: address })
    member
      (map-set members { address: address }
        (merge member {
          votes-cast: (+ (get votes-cast member) u1),
          last-active: block-height
        })
      )
    false
  )
)

;; ============================================================
;; MEMBER MANAGEMENT
;; ============================================================

;; Register a new member with a bootstrap reputation grant
(define-public (register-member (domain-tags (list 5 (string-ascii 32))))
  (begin
    (asserts! (not (is-member tx-sender)) ERR-ALREADY-MEMBER)
    (map-set members
      { address: tx-sender }
      {
        reputation: u50,
        last-active: block-height,
        proposals-made: u0,
        votes-cast: u0,
        correct-outcomes: u0,
        domain-tags: domain-tags
      }
    )
    (var-set total-reputation (+ (var-get total-reputation) u50))
    (ok true)
  )
)

;; Owner can grant additional reputation (e.g., for verified expertise)
(define-public (grant-reputation (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-member recipient) ERR-NOT-MEMBER)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (match (map-get? members { address: recipient })
      member
        (begin
          (map-set members { address: recipient }
            (merge member { reputation: (+ (get reputation member) amount) })
          )
          (var-set total-reputation (+ (var-get total-reputation) amount))
          (ok true)
        )
      ERR-NOT-MEMBER
    )
  )
)

;; Members can refresh their own reputation decay by calling this
(define-public (refresh-activity)
  (begin
    (asserts! (is-member tx-sender) ERR-NOT-MEMBER)
    (match (map-get? members { address: tx-sender })
      member
        (let ((decayed (apply-decay (get reputation member) (get last-active member))))
          (map-set members { address: tx-sender }
            (merge member {
              reputation: decayed,
              last-active: block-height
            })
          )
          (ok decayed)
        )
      ERR-NOT-MEMBER
    )
  )
)

;; ============================================================
;; PROPOSAL MANAGEMENT
;; ============================================================

;; Submit a new governance proposal
(define-public (submit-proposal
    (title (string-ascii 64))
    (description (string-ascii 256))
    (domain (string-ascii 32)))
  (let (
    (rep (effective-reputation tx-sender))
    (pid (+ (var-get proposal-count) u1))
  )
    (asserts! (is-member tx-sender) ERR-NOT-MEMBER)
    (asserts! (>= rep MIN-REPUTATION-TO-PROPOSE) ERR-NOT-AUTHORIZED)
    (map-set proposals { proposal-id: pid }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        domain: domain,
        created-at: block-height,
        voting-ends-at: (+ block-height VOTING-PERIOD),
        yes-votes: u0,
        no-votes: u0,
        yes-reputation: u0,
        no-reputation: u0,
        status: u0,
        executed: false
      }
    )
    (var-set proposal-count pid)
    ;; Reward proposer with small reputation bonus
    (match (map-get? members { address: tx-sender })
      member
        (map-set members { address: tx-sender }
          (merge member {
            proposals-made: (+ (get proposals-made member) u1),
            last-active: block-height
          })
        )
      false
    )
    (ok pid)
  )
)

;; ============================================================
;; VOTING
;; ============================================================

;; Cast votes on an active proposal using quadratic voting.
;; vote-units: number of vote units to spend (cost = units^2 reputation)
;; direction: true = yes, false = no
(define-public (cast-vote (proposal-id uint) (vote-units uint) (direction bool))
  (let (
    (rep (effective-reputation tx-sender))
    (cost (quadratic-cost vote-units))
  )
    (asserts! (is-member tx-sender) ERR-NOT-MEMBER)
    (asserts! (> vote-units u0) ERR-INVALID-AMOUNT)
    (asserts! (<= vote-units MAX-VOTES-PER-PROPOSAL) ERR-INVALID-AMOUNT)
    (asserts! (>= rep cost) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? member-votes { proposal-id: proposal-id, voter: tx-sender }))
      ERR-ALREADY-VOTED)
    (match (get-proposal proposal-id)
      proposal
        (begin
          (asserts! (is-eq (get status proposal) u0) ERR-PROPOSAL-EXECUTED)
          (asserts! (<= block-height (get voting-ends-at proposal)) ERR-PROPOSAL-EXPIRED)
          ;; Record the vote
          (map-set member-votes
            { proposal-id: proposal-id, voter: tx-sender }
            {
              direction: direction,
              votes-used: vote-units,
              reputation-at-vote: rep
            }
          )
          ;; Update proposal tallies
          (if direction
            (map-set proposals { proposal-id: proposal-id }
              (merge proposal {
                yes-votes: (+ (get yes-votes proposal) vote-units),
                yes-reputation: (+ (get yes-reputation proposal) cost)
              })
            )
            (map-set proposals { proposal-id: proposal-id }
              (merge proposal {
                no-votes: (+ (get no-votes proposal) vote-units),
                no-reputation: (+ (get no-reputation proposal) cost)
              })
            )
          )
          (record-activity tx-sender)
          (ok true)
        )
      ERR-PROPOSAL-NOT-FOUND
    )
  )
)

;; ============================================================
;; PROPOSAL FINALIZATION
;; ============================================================

;; Finalize a proposal after voting period ends.
;; Checks quorum and sets status to passed (1) or rejected (2).
(define-public (finalize-proposal (proposal-id uint))
  (begin
    (asserts! (is-member tx-sender) ERR-NOT-MEMBER)
    (match (get-proposal proposal-id)
      proposal
        (let (
          (total-rep (var-get total-reputation))
          (participation (+ (get yes-reputation proposal) (get no-reputation proposal)))
          (quorum-needed (/ (* total-rep QUORUM-PERCENT) u100))
        )
          (asserts! (is-eq (get status proposal) u0) ERR-PROPOSAL-EXECUTED)
          (asserts! (> block-height (get voting-ends-at proposal)) ERR-PROPOSAL-ACTIVE)
          (asserts! (>= participation quorum-needed) ERR-QUORUM-NOT-MET)
          (let (
            (passed (> (get yes-reputation proposal) (get no-reputation proposal)))
            (new-status (if passed u1 u2))
          )
            (map-set proposals { proposal-id: proposal-id }
              (merge proposal { status: new-status })
            )
            (ok new-status)
          )
        )
      ERR-PROPOSAL-NOT-FOUND
    )
  )
)

;; Execute a passed proposal (marks it as executed).
;; In production, this would trigger an external contract call.
(define-public (execute-proposal (proposal-id uint))
  (begin
    (asserts! (is-member tx-sender) ERR-NOT-MEMBER)
    (match (get-proposal proposal-id)
      proposal
        (begin
          (asserts! (is-eq (get status proposal) u1) ERR-VOTE-NOT-PASSED)
          (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXECUTED)
          (map-set proposals { proposal-id: proposal-id }
            (merge proposal { status: u3, executed: true })
          )
          (ok true)
        )
      ERR-PROPOSAL-NOT-FOUND
    )
  )
)

;; Cancel a proposal (proposer or owner only, while still active)
(define-public (cancel-proposal (proposal-id uint))
  (begin
    (match (get-proposal proposal-id)
      proposal
        (begin
          (asserts!
            (or (is-eq tx-sender (get proposer proposal))
                (is-eq tx-sender CONTRACT-OWNER))
            ERR-NOT-AUTHORIZED)
          (asserts! (is-eq (get status proposal) u0) ERR-PROPOSAL-EXECUTED)
          (map-set proposals { proposal-id: proposal-id }
            (merge proposal { status: u4 })
          )
          (ok true)
        )
      ERR-PROPOSAL-NOT-FOUND
    )
  )
)

;; ============================================================
;; MEDIATION POOL
;; ============================================================

;; Register as a mediator (requires elevated reputation)
(define-public (register-mediator)
  (let ((rep (effective-reputation tx-sender)))
    (asserts! (is-member tx-sender) ERR-NOT-MEMBER)
    (asserts! (>= rep u200) ERR-NOT-AUTHORIZED)
    (map-set mediators { address: tx-sender } { active: true, cases-resolved: u0 })
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY QUERIES
;; ============================================================

;; Get member info
(define-read-only (get-member (address principal))
  (map-get? members { address: address })
)

;; Get effective (decay-adjusted) reputation for any address
(define-read-only (get-effective-reputation (address principal))
  (ok (effective-reputation address))
)

;; Get proposal details
(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get a member's vote on a specific proposal
(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? member-votes { proposal-id: proposal-id, voter: voter })
)

;; Get total number of proposals submitted
(define-read-only (get-proposal-count)
  (ok (var-get proposal-count))
)

;; Get total reputation in the system
(define-read-only (get-total-reputation)
  (ok (var-get total-reputation))
)

;; Predict outcome of a proposal based on current vote tallies
;; Returns: { likely-pass: bool, yes-pct: uint, no-pct: uint, quorum-met: bool }
(define-read-only (predict-outcome (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal
      (let (
        (total-rep (var-get total-reputation))
        (yes-rep (get yes-reputation proposal))
        (no-rep (get no-reputation proposal))
        (participation (+ yes-rep no-rep))
        (quorum-needed (/ (* total-rep QUORUM-PERCENT) u100))
        (yes-pct (if (> participation u0) (/ (* yes-rep u100) participation) u0))
        (no-pct (if (> participation u0) (/ (* no-rep u100) participation) u0))
      )
        (ok {
          likely-pass: (> yes-rep no-rep),
          yes-pct: yes-pct,
          no-pct: no-pct,
          quorum-met: (>= participation quorum-needed),
          participation-rep: participation
        })
      )
    ERR-PROPOSAL-NOT-FOUND
  )
)
