(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PATENT_NOT_FOUND (err u101))
(define-constant ERR_PATENT_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u103))
(define-constant ERR_PATENT_EXPIRED (err u104))
(define-constant ERR_INVALID_DURATION (err u105))
(define-constant ERR_VOTING_ENDED (err u106))
(define-constant ERR_ALREADY_VOTED (err u107))
(define-constant ERR_INSUFFICIENT_STAKE (err u108))

(define-constant MIN_PATENT_DURATION u144)
(define-constant MAX_PATENT_DURATION u52560)
(define-constant MIN_USAGE_FEE u1000000)
(define-constant MIN_STAKE_AMOUNT u10000000)
(define-constant VOTING_PERIOD u1008)

(define-data-var next-patent-id uint u1)
(define-data-var dao-treasury uint u0)

(define-map patents
  { patent-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    usage-fee: uint,
    created-at: uint,
    expires-at: uint,
    total-revenue: uint,
    is-active: bool
  }
)

(define-map patent-usage
  { patent-id: uint, user: principal }
  {
    licensed-at: uint,
    usage-count: uint,
    total-paid: uint
  }
)

(define-map dao-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    patent-id: uint,
    proposal-type: (string-ascii 20),
    description: (string-ascii 300),
    created-at: uint,
    voting-ends-at: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, stake: uint }
)

(define-map user-stakes
  { user: principal }
  { amount: uint, locked-until: uint }
)

(define-data-var next-proposal-id uint u1)

(define-public (register-patent (title (string-ascii 100)) (description (string-ascii 500)) (usage-fee uint) (duration uint))
  (let
    (
      (patent-id (var-get next-patent-id))
      (current-block stacks-block-height)
    )
    (asserts! (>= duration MIN_PATENT_DURATION) ERR_INVALID_DURATION)
    (asserts! (<= duration MAX_PATENT_DURATION) ERR_INVALID_DURATION)
    (asserts! (>= usage-fee MIN_USAGE_FEE) ERR_INSUFFICIENT_PAYMENT)
    (asserts! (is-none (map-get? patents { patent-id: patent-id })) ERR_PATENT_ALREADY_EXISTS)
    
    (map-set patents
      { patent-id: patent-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        usage-fee: usage-fee,
        created-at: current-block,
        expires-at: (+ current-block duration),
        total-revenue: u0,
        is-active: true
      }
    )
    
    (var-set next-patent-id (+ patent-id u1))
    (ok patent-id)
  )
)

(define-public (license-patent (patent-id uint))
  (let
    (
      (patent (unwrap! (map-get? patents { patent-id: patent-id }) ERR_PATENT_NOT_FOUND))
      (current-block stacks-block-height)
      (usage-key { patent-id: patent-id, user: tx-sender })
      (existing-usage (map-get? patent-usage usage-key))
    )
    (asserts! (get is-active patent) ERR_PATENT_NOT_FOUND)
    (asserts! (< current-block (get expires-at patent)) ERR_PATENT_EXPIRED)
    
    (try! (stx-transfer? (get usage-fee patent) tx-sender (get owner patent)))
    
    (map-set patents
      { patent-id: patent-id }
      (merge patent { total-revenue: (+ (get total-revenue patent) (get usage-fee patent)) })
    )
    
    (match existing-usage
      prev-usage (map-set patent-usage
        usage-key
        {
          licensed-at: current-block,
          usage-count: (+ (get usage-count prev-usage) u1),
          total-paid: (+ (get total-paid prev-usage) (get usage-fee patent))
        }
      )
      (map-set patent-usage
        usage-key
        {
          licensed-at: current-block,
          usage-count: u1,
          total-paid: (get usage-fee patent)
        }
      )
    )
    
    (ok true)
  )
)

(define-public (stake-tokens (amount uint))
  (let
    (
      (current-block stacks-block-height)
      (user-key { user: tx-sender })
      (existing-stake (map-get? user-stakes user-key))
    )
    (asserts! (>= amount MIN_STAKE_AMOUNT) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (match existing-stake
      prev-stake (map-set user-stakes
        user-key
        {
          amount: (+ (get amount prev-stake) amount),
          locked-until: (+ current-block VOTING_PERIOD)
        }
      )
      (map-set user-stakes
        user-key
        {
          amount: amount,
          locked-until: (+ current-block VOTING_PERIOD)
        }
      )
    )
    
    (ok true)
  )
)

(define-public (create-proposal (patent-id uint) (proposal-type (string-ascii 20)) (description (string-ascii 300)))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (current-block stacks-block-height)
      (user-stake (map-get? user-stakes { user: tx-sender }))
    )
    (asserts! (is-some (map-get? patents { patent-id: patent-id })) ERR_PATENT_NOT_FOUND)
    (asserts! (is-some user-stake) ERR_INSUFFICIENT_STAKE)
    (asserts! (>= (get amount (unwrap-panic user-stake)) MIN_STAKE_AMOUNT) ERR_INSUFFICIENT_STAKE)
    
    (map-set dao-proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        patent-id: patent-id,
        proposal-type: proposal-type,
        description: description,
        created-at: current-block,
        voting-ends-at: (+ current-block VOTING_PERIOD),
        votes-for: u0,
        votes-against: u0,
        executed: false
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let
    (
      (proposal (unwrap! (map-get? dao-proposals { proposal-id: proposal-id }) ERR_PATENT_NOT_FOUND))
      (current-block stacks-block-height)
      (user-stake (unwrap! (map-get? user-stakes { user: tx-sender }) ERR_INSUFFICIENT_STAKE))
      (vote-key { proposal-id: proposal-id, voter: tx-sender })
    )
    (asserts! (< current-block (get voting-ends-at proposal)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? proposal-votes vote-key)) ERR_ALREADY_VOTED)
    (asserts! (>= (get amount user-stake) MIN_STAKE_AMOUNT) ERR_INSUFFICIENT_STAKE)
    
    (map-set proposal-votes
      vote-key
      { vote: vote, stake: (get amount user-stake) }
    )
    
    (map-set dao-proposals
      { proposal-id: proposal-id }
      (if vote
        (merge proposal { votes-for: (+ (get votes-for proposal) (get amount user-stake)) })
        (merge proposal { votes-against: (+ (get votes-against proposal) (get amount user-stake)) })
      )
    )
    
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? dao-proposals { proposal-id: proposal-id }) ERR_PATENT_NOT_FOUND))
      (current-block stacks-block-height)
      (patent (unwrap! (map-get? patents { patent-id: (get patent-id proposal) }) ERR_PATENT_NOT_FOUND))
    )
    (asserts! (>= current-block (get voting-ends-at proposal)) ERR_VOTING_ENDED)
    (asserts! (not (get executed proposal)) ERR_UNAUTHORIZED)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_UNAUTHORIZED)
    
    (map-set dao-proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    
    (if (is-eq (get proposal-type proposal) "deactivate")
      (map-set patents
        { patent-id: (get patent-id proposal) }
        (merge patent { is-active: false })
      )
      true
    )
    
    (ok true)
  )
)

(define-public (withdraw-stake)
  (let
    (
      (user-key { user: tx-sender })
      (user-stake (unwrap! (map-get? user-stakes user-key) ERR_INSUFFICIENT_STAKE))
      (current-block stacks-block-height)
    )
    (asserts! (>= current-block (get locked-until user-stake)) ERR_VOTING_ENDED)
    
    (try! (as-contract (stx-transfer? (get amount user-stake) tx-sender tx-sender)))
    (map-delete user-stakes user-key)
    
    (ok (get amount user-stake))
  )
)

(define-read-only (get-patent (patent-id uint))
  (map-get? patents { patent-id: patent-id })
)

(define-read-only (get-patent-usage (patent-id uint) (user principal))
  (map-get? patent-usage { patent-id: patent-id, user: user })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? dao-proposals { proposal-id: proposal-id })
)

(define-read-only (get-user-stake (user principal))
  (map-get? user-stakes { user: user })
)

(define-read-only (get-next-patent-id)
  (var-get next-patent-id)
)

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id)
)

(define-read-only (is-patent-active (patent-id uint))
  (match (map-get? patents { patent-id: patent-id })
    patent (and (get is-active patent) (< stacks-block-height (get expires-at patent)))
    false
  )
)

(define-read-only (get-dao-treasury)
  (var-get dao-treasury)
)