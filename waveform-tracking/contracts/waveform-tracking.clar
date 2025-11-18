;; WaveForm Supply Chain Tracking System
;; Blockchain-based product authentication and provenance tracking using electromagnetic signatures

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-signature (err u104))
(define-constant err-invalid-checkpoint (err u105))
(define-constant err-product-recalled (err u106))
(define-constant err-not-verified (err u107))

;; Data Variables
(define-data-var next-product-id uint u1)
(define-data-var next-checkpoint-id uint u1)
(define-data-var verification-reward uint u1000000) ;; 1 STX in microSTX

;; Data Maps

;; Product Registry - Digital twins of physical products
(define-map products
    { product-id: uint }
    {
        manufacturer: principal,
        batch-number: (string-ascii 64),
        product-type: (string-ascii 128),
        em-signature-hash: (buff 32), ;; Hash of electromagnetic signature
        manufacturing-date: uint,
        registration-height: uint,
        is-active: bool,
        is-recalled: bool,
        total-checkpoints: uint
    }
)

;; Electromagnetic signature verification data
(define-map em-signatures
    { product-id: uint }
    {
        signature-data: (string-ascii 512), ;; Compressed signature data
        confidence-score: uint, ;; 0-10000 (basis points)
        verified-by: principal,
        verification-height: uint
    }
)

;; Supply chain checkpoints
(define-map checkpoints
    { checkpoint-id: uint }
    {
        product-id: uint,
        checkpoint-type: (string-ascii 64), ;; manufacturing, warehouse, transit, retail, etc.
        location: (string-ascii 256),
        handler: principal,
        timestamp: uint,
        temperature: int, ;; For temperature-sensitive goods
        em-verified: bool,
        compliance-passed: bool,
        notes: (string-ascii 512)
    }
)

;; Product checkpoint history (maps product to checkpoints)
(define-map product-checkpoints
    { product-id: uint, checkpoint-index: uint }
    { checkpoint-id: uint }
)

;; Authorized verifiers and validators
(define-map authorized-verifiers
    { verifier: principal }
    {
        verifier-type: (string-ascii 64), ;; manufacturer, distributor, inspector, etc.
        authorized-by: principal,
        authorization-date: uint,
        is-active: bool
    }
)

;; Batch-level recall tracking
(define-map batch-recalls
    { batch-number: (string-ascii 64) }
    {
        recall-reason: (string-ascii 512),
        recall-date: uint,
        recalled-by: principal,
        affected-products: uint
    }
)

;; Consumer verification logs
(define-map consumer-verifications
    { product-id: uint, verifier: principal, verification-id: uint }
    {
        verification-date: uint,
        location: (string-ascii 256),
        result: bool
    }
)

;; Sustainability and compliance tokens
(define-map sustainability-scores
    { product-id: uint }
    {
        carbon-footprint: uint,
        sustainability-rating: uint, ;; 0-100
        certifications: (string-ascii 256),
        tokens-earned: uint
    }
)

;; Read-only functions

(define-read-only (get-product (product-id uint))
    (map-get? products { product-id: product-id })
)

(define-read-only (get-em-signature (product-id uint))
    (map-get? em-signatures { product-id: product-id })
)

(define-read-only (get-checkpoint (checkpoint-id uint))
    (map-get? checkpoints { checkpoint-id: checkpoint-id })
)

(define-read-only (get-product-checkpoint (product-id uint) (checkpoint-index uint))
    (map-get? product-checkpoints { product-id: product-id, checkpoint-index: checkpoint-index })
)

(define-read-only (is-authorized-verifier (verifier principal))
    (match (map-get? authorized-verifiers { verifier: verifier })
        verifier-data (get is-active verifier-data)
        false
    )
)

(define-read-only (get-batch-recall (batch-number (string-ascii 64)))
    (map-get? batch-recalls { batch-number: batch-number })
)

(define-read-only (get-sustainability-score (product-id uint))
    (map-get? sustainability-scores { product-id: product-id })
)

(define-read-only (get-verification-reward)
    (var-get verification-reward)
)

;; Public functions

;; Register a new product with electromagnetic signature
(define-public (register-product
    (batch-number (string-ascii 64))
    (product-type (string-ascii 128))
    (em-signature-hash (buff 32))
    (signature-data (string-ascii 512))
    (manufacturing-date uint))
    (let
        (
            (product-id (var-get next-product-id))
        )
        (asserts! (is-authorized-verifier tx-sender) err-unauthorized)
        (asserts! (is-none (get-product product-id)) err-already-exists)
        
        ;; Register product
        (map-set products
            { product-id: product-id }
            {
                manufacturer: tx-sender,
                batch-number: batch-number,
                product-type: product-type,
                em-signature-hash: em-signature-hash,
                manufacturing-date: manufacturing-date,
                registration-height: block-height,
                is-active: true,
                is-recalled: false,
                total-checkpoints: u0
            }
        )
        
        ;; Store electromagnetic signature
        (map-set em-signatures
            { product-id: product-id }
            {
                signature-data: signature-data,
                confidence-score: u10000, ;; 100% initial confidence
                verified-by: tx-sender,
                verification-height: block-height
            }
        )
        
        ;; Initialize sustainability tracking
        (map-set sustainability-scores
            { product-id: product-id }
            {
                carbon-footprint: u0,
                sustainability-rating: u0,
                certifications: "",
                tokens-earned: u0
            }
        )
        
        (var-set next-product-id (+ product-id u1))
        (ok product-id)
    )
)

;; Add a checkpoint in the supply chain
(define-public (add-checkpoint
    (product-id uint)
    (checkpoint-type (string-ascii 64))
    (location (string-ascii 256))
    (temperature int)
    (em-verified bool)
    (notes (string-ascii 512)))
    (let
        (
            (product (unwrap! (get-product product-id) err-not-found))
            (checkpoint-id (var-get next-checkpoint-id))
            (checkpoint-index (get total-checkpoints product))
        )
        (asserts! (is-authorized-verifier tx-sender) err-unauthorized)
        (asserts! (get is-active product) err-invalid-checkpoint)
        (asserts! (not (get is-recalled product)) err-product-recalled)
        
        ;; Create checkpoint
        (map-set checkpoints
            { checkpoint-id: checkpoint-id }
            {
                product-id: product-id,
                checkpoint-type: checkpoint-type,
                location: location,
                handler: tx-sender,
                timestamp: block-height,
                temperature: temperature,
                em-verified: em-verified,
                compliance-passed: true, ;; Default to true, can be updated
                notes: notes
            }
        )
        
        ;; Link checkpoint to product
        (map-set product-checkpoints
            { product-id: product-id, checkpoint-index: checkpoint-index }
            { checkpoint-id: checkpoint-id }
        )
        
        ;; Update product checkpoint count
        (map-set products
            { product-id: product-id }
            (merge product { total-checkpoints: (+ checkpoint-index u1) })
        )
        
        (var-set next-checkpoint-id (+ checkpoint-id u1))
        (ok checkpoint-id)
    )
)

;; Verify electromagnetic signature match
(define-public (verify-em-signature
    (product-id uint)
    (signature-hash (buff 32))
    (confidence-score uint))
    (let
        (
            (product (unwrap! (get-product product-id) err-not-found))
            (stored-signature (unwrap! (get-em-signature product-id) err-not-found))
        )
        (asserts! (is-authorized-verifier tx-sender) err-unauthorized)
        (asserts! (is-eq signature-hash (get em-signature-hash product)) err-invalid-signature)
        
        ;; Update signature verification data
        (map-set em-signatures
            { product-id: product-id }
            (merge stored-signature {
                confidence-score: confidence-score,
                verified-by: tx-sender,
                verification-height: block-height
            })
        )
        
        ;; Reward verifier for successful verification
        (if (>= confidence-score u9000) ;; 90% confidence threshold
            (try! (stx-transfer? (var-get verification-reward) (as-contract tx-sender) tx-sender))
            true
        )
        
        (ok true)
    )
)

;; Consumer verification function
(define-public (verify-product-authenticity
    (product-id uint)
    (location (string-ascii 256)))
    (let
        (
            (product (unwrap! (get-product product-id) err-not-found))
            (signature (unwrap! (get-em-signature product-id) err-not-found))
            (verification-id (get total-checkpoints product))
        )
        (asserts! (get is-active product) err-not-verified)
        (asserts! (not (get is-recalled product)) err-product-recalled)
        (asserts! (>= (get confidence-score signature) u8000) err-invalid-signature)
        
        ;; Log consumer verification
        (map-set consumer-verifications
            { product-id: product-id, verifier: tx-sender, verification-id: verification-id }
            {
                verification-date: block-height,
                location: location,
                result: true
            }
        )
        
        (ok true)
    )
)

;; Initiate product recall
(define-public (initiate-recall
    (product-id uint)
    (batch-number (string-ascii 64))
    (recall-reason (string-ascii 512)))
    (let
        (
            (product (unwrap! (get-product product-id) err-not-found))
        )
        (asserts! (or 
            (is-eq tx-sender (get manufacturer product))
            (is-eq tx-sender contract-owner)
        ) err-unauthorized)
        
        ;; Mark product as recalled
        (map-set products
            { product-id: product-id }
            (merge product { is-recalled: true, is-active: false })
        )
        
        ;; Record batch recall
        (map-set batch-recalls
            { batch-number: batch-number }
            {
                recall-reason: recall-reason,
                recall-date: block-height,
                recalled-by: tx-sender,
                affected-products: u1 ;; Would be calculated in production
            }
        )
        
        (ok true)
    )
)

;; Update compliance status for a checkpoint
(define-public (update-checkpoint-compliance
    (checkpoint-id uint)
    (compliance-passed bool))
    (let
        (
            (checkpoint (unwrap! (get-checkpoint checkpoint-id) err-not-found))
        )
        (asserts! (is-authorized-verifier tx-sender) err-unauthorized)
        
        (map-set checkpoints
            { checkpoint-id: checkpoint-id }
            (merge checkpoint { compliance-passed: compliance-passed })
        )
        (ok true)
    )
)

;; Update sustainability metrics and award tokens
(define-public (update-sustainability-score
    (product-id uint)
    (carbon-footprint uint)
    (sustainability-rating uint)
    (certifications (string-ascii 256)))
    (let
        (
            (product (unwrap! (get-product product-id) err-not-found))
            (current-score (unwrap! (get-sustainability-score product-id) err-not-found))
            (tokens-to-award (if (> sustainability-rating u80) u100 u0))
        )
        (asserts! (is-authorized-verifier tx-sender) err-unauthorized)
        (asserts! (<= sustainability-rating u100) err-invalid-checkpoint)
        
        (map-set sustainability-scores
            { product-id: product-id }
            {
                carbon-footprint: carbon-footprint,
                sustainability-rating: sustainability-rating,
                certifications: certifications,
                tokens-earned: (+ (get tokens-earned current-score) tokens-to-award)
            }
        )
        (ok tokens-to-award)
    )
)

;; Authorize a new verifier
(define-public (authorize-verifier
    (verifier principal)
    (verifier-type (string-ascii 64)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-verifiers
            { verifier: verifier }
            {
                verifier-type: verifier-type,
                authorized-by: tx-sender,
                authorization-date: block-height,
                is-active: true
            }
        )
        (ok true)
    )
)

;; Revoke verifier authorization
(define-public (revoke-verifier (verifier principal))
    (let
        (
            (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: verifier }) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-verifiers
            { verifier: verifier }
            (merge verifier-data { is-active: false })
        )
        (ok true)
    )
)

;; Update verification reward amount
(define-public (set-verification-reward (new-reward uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set verification-reward new-reward)
        (ok true)
    )
)