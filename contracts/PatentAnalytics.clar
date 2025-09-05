;; Patent Analytics & Performance Tracking Contract
;; Provides comprehensive analytics for patent usage, revenue, and performance metrics

(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_PATENT_NOT_FOUND (err u201))
(define-constant ERR_INVALID_TIMEFRAME (err u202))
(define-constant ERR_NO_DATA (err u203))

;; Constants for analytics periods
(define-constant DAILY_BLOCKS u144)  ;; ~24 hours
(define-constant WEEKLY_BLOCKS u1008) ;; ~7 days
(define-constant MONTHLY_BLOCKS u4320) ;; ~30 days

;; Data variables
(define-data-var total-licenses-issued uint u0)
(define-data-var total-revenue-generated uint u0)

;; Maps for tracking analytics
(define-map patent-daily-stats
  { patent-id: uint, day: uint }
  {
    licenses-count: uint,
    revenue: uint,
    unique-users: uint
  }
)

(define-map patent-performance-metrics
  { patent-id: uint }
  {
    total-licenses: uint,
    total-revenue: uint,
    average-daily-usage: uint,
    peak-usage-day: uint,
    first-license-at: uint,
    last-license-at: uint,
    unique-licensees: uint,
    performance-score: uint
  }
)

(define-map daily-user-usage
  { patent-id: uint, user: principal, day: uint }
  { licensed: bool }
)

(define-map patent-rankings
  { rank-type: (string-ascii 20) }
  {
    top-patent-id: uint,
    top-value: uint,
    updated-at: uint
  }
)

(define-map user-analytics
  { user: principal }
  {
    total-licenses-purchased: uint,
    total-spent: uint,
    favorite-patent-category: (string-ascii 50),
    first-license-at: uint,
    last-license-at: uint
  }
)

;; Record analytics when a patent is licensed
(define-public (record-license-analytics (patent-id uint) (user principal) (fee uint))
  (let
    (
      (current-block stacks-block-height)
      (current-day (/ current-block DAILY_BLOCKS))
      (daily-key { patent-id: patent-id, day: current-day })
      (user-daily-key { patent-id: patent-id, user: user, day: current-day })
      (performance-key { patent-id: patent-id })
      (user-key { user: user })
      (existing-daily (default-to { licenses-count: u0, revenue: u0, unique-users: u0 } 
                                  (map-get? patent-daily-stats daily-key)))
      (existing-performance (map-get? patent-performance-metrics performance-key))
      (existing-user-analytics (map-get? user-analytics user-key))
      (is-new-user-today (is-none (map-get? daily-user-usage user-daily-key)))
    )
    
    ;; Update daily statistics
    (map-set patent-daily-stats
      daily-key
      {
        licenses-count: (+ (get licenses-count existing-daily) u1),
        revenue: (+ (get revenue existing-daily) fee),
        unique-users: (if is-new-user-today 
                         (+ (get unique-users existing-daily) u1) 
                         (get unique-users existing-daily))
      }
    )
    
    ;; Mark user as having used patent today
    (if is-new-user-today
      (map-set daily-user-usage user-daily-key { licensed: true })
      true
    )
    
    ;; Update performance metrics
    (match existing-performance
      prev-metrics (map-set patent-performance-metrics
        performance-key
        {
          total-licenses: (+ (get total-licenses prev-metrics) u1),
          total-revenue: (+ (get total-revenue prev-metrics) fee),
          average-daily-usage: (let ((days-elapsed (/ (- current-block (get first-license-at prev-metrics)) DAILY_BLOCKS)))
                                  (/ (+ (get total-licenses prev-metrics) u1) (if (> days-elapsed u0) days-elapsed u1))),
          peak-usage-day: (if (> (+ (get licenses-count existing-daily) u1) 
                                 (get-peak-daily-usage patent-id))
                             current-day
                             (get peak-usage-day prev-metrics)),
          first-license-at: (get first-license-at prev-metrics),
          last-license-at: current-block,
          unique-licensees: (if is-new-user-today 
                               (+ (get unique-licensees prev-metrics) u1) 
                               (get unique-licensees prev-metrics)),
          performance-score: (calculate-performance-score patent-id)
        }
      )
      (map-set patent-performance-metrics
        performance-key
        {
          total-licenses: u1,
          total-revenue: fee,
          average-daily-usage: u1,
          peak-usage-day: current-day,
          first-license-at: current-block,
          last-license-at: current-block,
          unique-licensees: u1,
          performance-score: u50
        }
      )
    )
    
    ;; Update user analytics
    (match existing-user-analytics
      prev-user (map-set user-analytics
        user-key
        {
          total-licenses-purchased: (+ (get total-licenses-purchased prev-user) u1),
          total-spent: (+ (get total-spent prev-user) fee),
          favorite-patent-category: (get favorite-patent-category prev-user),
          first-license-at: (get first-license-at prev-user),
          last-license-at: current-block
        }
      )
      (map-set user-analytics
        user-key
        {
          total-licenses-purchased: u1,
          total-spent: fee,
          favorite-patent-category: "general",
          first-license-at: current-block,
          last-license-at: current-block
        }
      )
    )
    
    ;; Update global statistics
    (var-set total-licenses-issued (+ (var-get total-licenses-issued) u1))
    (var-set total-revenue-generated (+ (var-get total-revenue-generated) fee))
    
    (ok true)
  )
)

;; Calculate performance score based on usage and revenue
(define-private (calculate-performance-score (patent-id uint))
  (let
    (
      (metrics (map-get? patent-performance-metrics { patent-id: patent-id }))
    )
    (match metrics
      data (let
        (
          (revenue-score (if (> (/ (get total-revenue data) u10000000) u50) u50 (/ (get total-revenue data) u10000000))) ;; Max 50 points for revenue
          (usage-score (if (> (get total-licenses data) u30) u30 (get total-licenses data))) ;; Max 30 points for usage
          (uniqueness-score (if (> (get unique-licensees data) u20) u20 (get unique-licensees data))) ;; Max 20 points for unique users
        )
        (+ revenue-score usage-score uniqueness-score)
      )
      u0
    )
  )
)

;; Get peak daily usage for a patent
(define-private (get-peak-daily-usage (patent-id uint))
  (let
    (
      (metrics (map-get? patent-performance-metrics { patent-id: patent-id }))
    )
    (match metrics
      data (let
        (
          (peak-day (get peak-usage-day data))
          (peak-stats (map-get? patent-daily-stats { patent-id: patent-id, day: peak-day }))
        )
        (match peak-stats
          stats (get licenses-count stats)
          u0
        )
      )
      u0
    )
  )
)

;; Public function to update patent rankings
(define-public (update-patent-rankings)
  (let
    (
      (current-block stacks-block-height)
    )
    ;; This is a simplified ranking update - in practice, you'd iterate through patents
    ;; For demonstration, we'll update the ranking structure
    (map-set patent-rankings
      { rank-type: "most-revenue" }
      {
        top-patent-id: u1, ;; Would be calculated by finding max revenue
        top-value: u0,
        updated-at: current-block
      }
    )
    
    (map-set patent-rankings
      { rank-type: "most-licensed" }
      {
        top-patent-id: u1, ;; Would be calculated by finding max licenses
        top-value: u0,
        updated-at: current-block
      }
    )
    
    (ok true)
  )
)

;; Get daily statistics for a patent
(define-read-only (get-patent-daily-stats (patent-id uint) (day uint))
  (map-get? patent-daily-stats { patent-id: patent-id, day: day })
)

;; Get performance metrics for a patent
(define-read-only (get-patent-performance (patent-id uint))
  (map-get? patent-performance-metrics { patent-id: patent-id })
)

;; Get usage trends for a patent over a period
(define-read-only (get-patent-trends (patent-id uint) (days-back uint))
  (let
    (
      (current-day (/ stacks-block-height DAILY_BLOCKS))
      (start-day (- current-day days-back))
    )
    ;; Return trend data structure (simplified for demo)
    {
      start-day: start-day,
      end-day: current-day,
      total-days: days-back,
      avg-daily-revenue: (get-average-daily-revenue patent-id days-back),
      avg-daily-licenses: (get-average-daily-licenses patent-id days-back)
    }
  )
)

;; Calculate average daily revenue over period
(define-private (get-average-daily-revenue (patent-id uint) (days uint))
  (let
    (
      (performance (map-get? patent-performance-metrics { patent-id: patent-id }))
    )
    (match performance
      data (/ (get total-revenue data) (if (> days u0) days u1))
      u0
    )
  )
)

;; Calculate average daily licenses over period
(define-private (get-average-daily-licenses (patent-id uint) (days uint))
  (let
    (
      (performance (map-get? patent-performance-metrics { patent-id: patent-id }))
    )
    (match performance
      data (/ (get total-licenses data) (if (> days u0) days u1))
      u0
    )
  )
)

;; Get user analytics
(define-read-only (get-user-analytics (user principal))
  (map-get? user-analytics { user: user })
)

;; Get patent ranking by type
(define-read-only (get-patent-ranking (rank-type (string-ascii 20)))
  (map-get? patent-rankings { rank-type: rank-type })
)

;; Get global platform statistics
(define-read-only (get-platform-stats)
  {
    total-licenses: (var-get total-licenses-issued),
    total-revenue: (var-get total-revenue-generated),
    avg-license-value: (if (> (var-get total-licenses-issued) u0)
                          (/ (var-get total-revenue-generated) (var-get total-licenses-issued))
                          u0)
  }
)

;; Compare patent performance against platform average
(define-read-only (get-patent-performance-comparison (patent-id uint))
  (let
    (
      (patent-metrics (map-get? patent-performance-metrics { patent-id: patent-id }))
      (platform-stats (get-platform-stats))
    )
    (match patent-metrics
      data {
        patent-avg-revenue: (if (> (get total-licenses data) u0)
                               (/ (get total-revenue data) (get total-licenses data))
                               u0),
        platform-avg-revenue: (get avg-license-value platform-stats),
        performance-vs-average: (if (and (> (get total-licenses data) u0) 
                                        (> (get avg-license-value platform-stats) u0))
                                   (/ (* (/ (get total-revenue data) (get total-licenses data)) u100) 
                                      (get avg-license-value platform-stats))
                                   u100),
        total-licenses: (get total-licenses data),
        performance-score: (get performance-score data)
      }
      {
        patent-avg-revenue: u0,
        platform-avg-revenue: (get avg-license-value platform-stats),
        performance-vs-average: u0,
        total-licenses: u0,
        performance-score: u0
      }
    )
  )
)
