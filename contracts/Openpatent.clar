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
(define-constant ERR_DISPUTE_NOT_FOUND (err u109))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u110))
(define-constant ERR_DISPUTE_CLOSED (err u111))
(define-constant ERR_NOT_ARBITRATOR (err u112))
(define-constant ERR_ALREADY_RESPONDED (err u113))
(define-constant ERR_DISPUTE_ACTIVE (err u114))
(define-constant ERR_INSUFFICIENT_BOND (err u115))

(define-constant MIN_PATENT_DURATION u144)
(define-constant MAX_PATENT_DURATION u52560)
(define-constant MIN_USAGE_FEE u1000000)
(define-constant MIN_STAKE_AMOUNT u10000000)
(define-constant VOTING_PERIOD u1008)
(define-constant DISPUTE_BOND_AMOUNT u50000000)
(define-constant ARBITRATION_PERIOD u504)
(define-constant ARBITRATOR_REWARD_PERCENT u10)

(define-data-var next-patent-id uint u1)
(define-data-var dao-treasury uint u0)
(define-data-var next-dispute-id uint u1)

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

(define-map patent-disputes
  { dispute-id: uint }
  {
    disputant: principal,
    patent-id: uint,
    patent-owner: principal,
    dispute-type: (string-ascii 30),
    reason: (string-ascii 500),
    evidence-hash: (string-ascii 64),
    created-at: uint,
    arbitration-deadline: uint,
    arbitrator: (optional principal),
    disputant-bond: uint,
    owner-bond: uint,
    status: (string-ascii 20),
    ruling: (optional bool),
    compensation-amount: uint
  }
)

(define-map dispute-responses
  { dispute-id: uint }
  {
    response: (string-ascii 500),
    counter-evidence-hash: (string-ascii 64),
    submitted-at: uint
  }
)

(define-map arbitrators
  { arbitrator: principal }
  {
    active: bool,
    cases-handled: uint,
    reputation-score: uint,
    locked-stake: uint
  }
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

(define-public (file-patent-dispute (patent-id uint) (dispute-type (string-ascii 30)) (reason (string-ascii 500)) (evidence-hash (string-ascii 64)))
  (let
    (
      (dispute-id (var-get next-dispute-id))
      (patent (unwrap! (map-get? patents { patent-id: patent-id }) ERR_PATENT_NOT_FOUND))
      (current-block stacks-block-height)
      (existing-dispute (map-get? patent-disputes { dispute-id: dispute-id }))
    )
    (asserts! (not (is-eq tx-sender (get owner patent))) ERR_UNAUTHORIZED)
    (asserts! (is-none existing-dispute) ERR_DISPUTE_ALREADY_EXISTS)
    
    (try! (stx-transfer? DISPUTE_BOND_AMOUNT tx-sender (as-contract tx-sender)))
    
    (map-set patent-disputes
      { dispute-id: dispute-id }
      {
        disputant: tx-sender,
        patent-id: patent-id,
        patent-owner: (get owner patent),
        dispute-type: dispute-type,
        reason: reason,
        evidence-hash: evidence-hash,
        created-at: current-block,
        arbitration-deadline: (+ current-block ARBITRATION_PERIOD),
        arbitrator: none,
        disputant-bond: DISPUTE_BOND_AMOUNT,
        owner-bond: u0,
        status: "open",
        ruling: none,
        compensation-amount: u0
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (respond-to-dispute (dispute-id uint) (response (string-ascii 500)) (counter-evidence-hash (string-ascii 64)))
  (let
    (
      (dispute (unwrap! (map-get? patent-disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (current-block stacks-block-height)
      (existing-response (map-get? dispute-responses { dispute-id: dispute-id }))
    )
    (asserts! (is-eq tx-sender (get patent-owner dispute)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status dispute) "open") ERR_DISPUTE_CLOSED)
    (asserts! (is-none existing-response) ERR_ALREADY_RESPONDED)
    (asserts! (< current-block (get arbitration-deadline dispute)) ERR_VOTING_ENDED)
    
    (try! (stx-transfer? DISPUTE_BOND_AMOUNT tx-sender (as-contract tx-sender)))
    
    (map-set dispute-responses
      { dispute-id: dispute-id }
      {
        response: response,
        counter-evidence-hash: counter-evidence-hash,
        submitted-at: current-block
      }
    )
    
    (map-set patent-disputes
      { dispute-id: dispute-id }
      (merge dispute {
        owner-bond: DISPUTE_BOND_AMOUNT,
        status: "awaiting-arbitration"
      })
    )
    
    (ok true)
  )
)

(define-public (register-arbitrator)
  (let
    (
      (arbitrator-key { arbitrator: tx-sender })
      (existing-arbitrator (map-get? arbitrators arbitrator-key))
      (user-stake (unwrap! (map-get? user-stakes { user: tx-sender }) ERR_INSUFFICIENT_STAKE))
    )
    (asserts! (>= (get amount user-stake) (* MIN_STAKE_AMOUNT u5)) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none existing-arbitrator) ERR_DISPUTE_ALREADY_EXISTS)
    
    (map-set arbitrators
      arbitrator-key
      {
        active: true,
        cases-handled: u0,
        reputation-score: u100,
        locked-stake: (get amount user-stake)
      }
    )
    
    (ok true)
  )
)

(define-public (assign-arbitrator (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? patent-disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR_NOT_ARBITRATOR))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (get status dispute) "awaiting-arbitration") ERR_DISPUTE_CLOSED)
    (asserts! (get active arbitrator-data) ERR_NOT_ARBITRATOR)
    (asserts! (is-none (get arbitrator dispute)) ERR_DISPUTE_ALREADY_EXISTS)
    (asserts! (< current-block (get arbitration-deadline dispute)) ERR_VOTING_ENDED)
    
    (map-set patent-disputes
      { dispute-id: dispute-id }
      (merge dispute {
        arbitrator: (some tx-sender),
        status: "under-arbitration"
      })
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint) (ruling bool) (compensation-amount uint))
  (let
    (
      (dispute (unwrap! (map-get? patent-disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR_NOT_ARBITRATOR))
      (current-block stacks-block-height)
      (arbitrator-reward (/ (* (+ (get disputant-bond dispute) (get owner-bond dispute)) ARBITRATOR_REWARD_PERCENT) u100))
      (total-bonds (+ (get disputant-bond dispute) (get owner-bond dispute)))
    )
    (asserts! (is-eq (get status dispute) "under-arbitration") ERR_DISPUTE_CLOSED)
    (asserts! (is-eq (some tx-sender) (get arbitrator dispute)) ERR_NOT_ARBITRATOR)
    (asserts! (>= current-block (get arbitration-deadline dispute)) ERR_VOTING_ENDED)
    
    (map-set patent-disputes
      { dispute-id: dispute-id }
      (merge dispute {
        status: "resolved",
        ruling: (some ruling),
        compensation-amount: compensation-amount
      })
    )
    
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge arbitrator-data {
        cases-handled: (+ (get cases-handled arbitrator-data) u1),
        reputation-score: (if ruling 
          (+ (get reputation-score arbitrator-data) u10)
          (+ (get reputation-score arbitrator-data) u5)
        )
      })
    )
    
    (if ruling
      (begin
        (try! (as-contract (stx-transfer? (get disputant-bond dispute) tx-sender (get disputant dispute))))
        (try! (as-contract (stx-transfer? compensation-amount tx-sender (get disputant dispute))))
        (try! (as-contract (stx-transfer? arbitrator-reward tx-sender tx-sender)))
        (try! (as-contract (stx-transfer? (- (get owner-bond dispute) compensation-amount arbitrator-reward) tx-sender (get patent-owner dispute))))
      )
      (begin
        (try! (as-contract (stx-transfer? (get owner-bond dispute) tx-sender (get patent-owner dispute))))
        (try! (as-contract (stx-transfer? arbitrator-reward tx-sender tx-sender)))
        (try! (as-contract (stx-transfer? (- (get disputant-bond dispute) arbitrator-reward) tx-sender (get patent-owner dispute))))
      )
    )
    
    (ok ruling)
  )
)

(define-public (withdraw-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? patent-disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get disputant dispute)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status dispute) "open") ERR_DISPUTE_ACTIVE)
    (asserts! (is-none (map-get? dispute-responses { dispute-id: dispute-id })) ERR_ALREADY_RESPONDED)
    
    (map-set patent-disputes
      { dispute-id: dispute-id }
      (merge dispute { status: "withdrawn" })
    )
    
    (try! (as-contract (stx-transfer? (get disputant-bond dispute) tx-sender (get disputant dispute))))
    
    (ok true)
  )
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? patent-disputes { dispute-id: dispute-id })
)

(define-read-only (get-dispute-response (dispute-id uint))
  (map-get? dispute-responses { dispute-id: dispute-id })
)

(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-next-dispute-id)
  (var-get next-dispute-id)
)

(define-read-only (get-dao-treasury)
  (var-get dao-treasury)
)