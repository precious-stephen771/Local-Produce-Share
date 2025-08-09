;; ========================================
;; PRODUCE SHARE CONTRACT
;; Community Supported Agriculture Coordination System
;; ========================================

;; Contract for managing local produce sharing, harvest distribution,
;; payment scheduling, and delivery coordination

;; ========================================
;; CONSTANTS & ERROR CODES
;; ========================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_DELIVERY_NOT_READY (err u105))
(define-constant ERR_PAYMENT_OVERDUE (err u106))
(define-constant ERR_SHARE_FULL (err u107))
(define-constant ERR_INVALID_STATUS (err u108))

;; ========================================
;; DATA STRUCTURES
;; ========================================

;; Farm/Producer information
(define-map farms
  { farm-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    location: (string-ascii 100),
    active: bool,
    total-shares: uint,
    price-per-share: uint,
    created-at: uint
  }
)

;; Share subscription information
(define-map shares
  { share-id: uint }
  {
    farm-id: uint,
    member: principal,
    shares-count: uint,
    payment-schedule: (string-ascii 20), ;; "weekly", "monthly", "seasonal"
    next-payment-due: uint,
    total-paid: uint,
    status: (string-ascii 20), ;; "active", "suspended", "completed"
    delivery-route: uint,
    created-at: uint
  }
)

;; Harvest information
(define-map harvests
  { harvest-id: uint }
  {
    farm-id: uint,
    produce-type: (string-ascii 30),
    quantity: uint,
    harvest-date: uint,
    distribution-date: uint,
    status: (string-ascii 20), ;; "harvested", "distributed", "completed"
    shares-allocated: uint
  }
)

;; Delivery routes
(define-map delivery-routes
  { route-id: uint }
  {
    name: (string-ascii 50),
    driver: (optional principal),
    max-capacity: uint,
    current-load: uint,
    delivery-day: (string-ascii 10), ;; "monday", "tuesday", etc.
    active: bool
  }
)

;; Payment records
(define-map payments
  { payment-id: uint }
  {
    share-id: uint,
    member: principal,
    amount: uint,
    payment-date: uint,
    due-date: uint,
    status: (string-ascii 20) ;; "pending", "paid", "overdue"
  }
)

;; Distribution allocations
(define-map distributions
  { distribution-id: uint }
  {
    harvest-id: uint,
    share-id: uint,
    member: principal,
    allocated-quantity: uint,
    pickup-status: (string-ascii 20), ;; "ready", "picked-up", "missed"
    pickup-date: (optional uint)
  }
)

;; ========================================
;; DATA VARIABLES
;; ========================================

(define-data-var next-farm-id uint u1)
(define-data-var next-share-id uint u1)
(define-data-var next-harvest-id uint u1)
(define-data-var next-route-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var next-distribution-id uint u1)

;; ========================================
;; FARM MANAGEMENT FUNCTIONS
;; ========================================

(define-public (register-farm (name (string-ascii 50)) (location (string-ascii 100)) (total-shares uint) (price-per-share uint))
  (let ((farm-id (var-get next-farm-id)))
    (asserts! (> total-shares u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-share u0) ERR_INVALID_AMOUNT)
    (map-set farms
      { farm-id: farm-id }
      {
        owner: tx-sender,
        name: name,
        location: location,
        active: true,
        total-shares: total-shares,
        price-per-share: price-per-share,
        created-at: stacks-block-height
      }
    )
    (var-set next-farm-id (+ farm-id u1))
    (ok farm-id)
  )
)

(define-public (update-farm-status (farm-id uint) (active bool))
  (let ((farm-data (unwrap! (map-get? farms { farm-id: farm-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get owner farm-data)) ERR_UNAUTHORIZED)
    (map-set farms
      { farm-id: farm-id }
      (merge farm-data { active: active })
    )
    (ok true)
  )
)

;; ========================================
;; SHARE SUBSCRIPTION FUNCTIONS
;; ========================================

(define-public (subscribe-to-share
  (farm-id uint)
  (shares-count uint)
  (payment-schedule (string-ascii 20))
  (delivery-route uint)
)
  (let (
    (share-id (var-get next-share-id))
    (farm-data (unwrap! (map-get? farms { farm-id: farm-id }) ERR_NOT_FOUND))
    (next-due (calculate-next-payment-date payment-schedule))
  )
    (asserts! (get active farm-data) ERR_NOT_FOUND)
    (asserts! (> shares-count u0) ERR_INVALID_AMOUNT)

    ;; Create share subscription
    (map-set shares
      { share-id: share-id }
      {
        farm-id: farm-id,
        member: tx-sender,
        shares-count: shares-count,
        payment-schedule: payment-schedule,
        next-payment-due: next-due,
        total-paid: u0,
        status: "active",
        delivery-route: delivery-route,
        created-at: stacks-block-height
      }
    )

    ;; Create initial payment record
    (let ((payment-id (var-get next-payment-id))
          (amount (* shares-count (get price-per-share farm-data))))
      (map-set payments
        { payment-id: payment-id }
        {
          share-id: share-id,
          member: tx-sender,
          amount: amount,
          payment-date: u0,
          due-date: next-due,
          status: "pending"
        }
      )
      (var-set next-payment-id (+ payment-id u1))
    )

    (var-set next-share-id (+ share-id u1))
    (ok share-id)
  )
)

;; ========================================
;; PAYMENT FUNCTIONS
;; ========================================

(define-public (process-payment (payment-id uint))
  (let (
    (payment-data (unwrap! (map-get? payments { payment-id: payment-id }) ERR_NOT_FOUND))
    (share-data (unwrap! (map-get? shares { share-id: (get share-id payment-data) }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get member payment-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status payment-data) "pending") ERR_INVALID_STATUS)

    ;; Transfer payment (in a real implementation, this would handle STX transfer)
    ;; For now, we'll mark as paid and update records
    (map-set payments
      { payment-id: payment-id }
      (merge payment-data {
        status: "paid",
        payment-date: stacks-block-height
      })
    )

    ;; Update share total paid
    (map-set shares
      { share-id: (get share-id payment-data) }
      (merge share-data {
        total-paid: (+ (get total-paid share-data) (get amount payment-data))
      })
    )

    (ok true)
  )
)

(define-private (calculate-next-payment-date (schedule (string-ascii 20)))
  ;; Simplified calculation - in production, this would be more sophisticated
  (if (is-eq schedule "weekly")
    (+ stacks-block-height u1008) ;; ~1 week in blocks
    (if (is-eq schedule "monthly")
      (+ stacks-block-height u4320) ;; ~1 month in blocks
      (+ stacks-block-height u25920) ;; ~6 months in blocks (seasonal)
    )
  )
)

;; ========================================
;; HARVEST MANAGEMENT FUNCTIONS
;; ========================================

(define-public (record-harvest
  (farm-id uint)
  (produce-type (string-ascii 30))
  (quantity uint)
  (distribution-date uint)
)
  (let (
    (harvest-id (var-get next-harvest-id))
    (farm-data (unwrap! (map-get? farms { farm-id: farm-id }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get owner farm-data)) ERR_UNAUTHORIZED)
    (asserts! (> quantity u0) ERR_INVALID_AMOUNT)

    (map-set harvests
      { harvest-id: harvest-id }
      {
        farm-id: farm-id,
        produce-type: produce-type,
        quantity: quantity,
        harvest-date: stacks-block-height,
        distribution-date: distribution-date,
        status: "harvested",
        shares-allocated: u0
      }
    )

    (var-set next-harvest-id (+ harvest-id u1))
    (ok harvest-id)
  )
)

(define-public (distribute-harvest (harvest-id uint))
  (let (
    (harvest-data (unwrap! (map-get? harvests { harvest-id: harvest-id }) ERR_NOT_FOUND))
    (farm-data (unwrap! (map-get? farms { farm-id: (get farm-id harvest-data) }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get owner farm-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status harvest-data) "harvested") ERR_INVALID_STATUS)
    (asserts! (>= stacks-block-height (get distribution-date harvest-data)) ERR_DELIVERY_NOT_READY)

    ;; Update harvest status
    (map-set harvests
      { harvest-id: harvest-id }
      (merge harvest-data { status: "distributed" })
    )

    ;; In a full implementation, this would iterate through all shares
    ;; and create distribution allocations
    (ok true)
  )
)

;; ========================================
;; DELIVERY ROUTE FUNCTIONS
;; ========================================

(define-public (create-delivery-route
  (name (string-ascii 50))
  (max-capacity uint)
  (delivery-day (string-ascii 10))
)
  (let ((route-id (var-get next-route-id)))
    (asserts! (> max-capacity u0) ERR_INVALID_AMOUNT)

    (map-set delivery-routes
      { route-id: route-id }
      {
        name: name,
        driver: none,
        max-capacity: max-capacity,
        current-load: u0,
        delivery-day: delivery-day,
        active: true
      }
    )

    (var-set next-route-id (+ route-id u1))
    (ok route-id)
  )
)

(define-public (assign-driver (route-id uint) (driver principal))
  (let ((route-data (unwrap! (map-get? delivery-routes { route-id: route-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

    (map-set delivery-routes
      { route-id: route-id }
      (merge route-data { driver: (some driver) })
    )
    (ok true)
  )
)

;; ========================================
;; READ-ONLY FUNCTIONS
;; ========================================

(define-read-only (get-farm (farm-id uint))
  (map-get? farms { farm-id: farm-id })
)

(define-read-only (get-share (share-id uint))
  (map-get? shares { share-id: share-id })
)

(define-read-only (get-harvest (harvest-id uint))
  (map-get? harvests { harvest-id: harvest-id })
)

(define-read-only (get-delivery-route (route-id uint))
  (map-get? delivery-routes { route-id: route-id })
)

(define-read-only (get-payment (payment-id uint))
  (map-get? payments { payment-id: payment-id })
)

(define-read-only (get-member-shares (member principal))
  ;; In a full implementation, this would return all shares for a member
  ;; For now, returns a simple confirmation
  (ok member)
)

(define-read-only (get-farm-harvests (farm-id uint))
  ;; In a full implementation, this would return all harvests for a farm
  ;; For now, returns the farm-id for confirmation
  (ok farm-id)
)

;; ========================================
;; UTILITY FUNCTIONS
;; ========================================

(define-read-only (get-contract-info)
  {
    next-farm-id: (var-get next-farm-id),
    next-share-id: (var-get next-share-id),
    next-harvest-id: (var-get next-harvest-id),
    next-route-id: (var-get next-route-id),
    next-payment-id: (var-get next-payment-id),
    next-distribution-id: (var-get next-distribution-id),
    current-block: stacks-block-height
  }
)
