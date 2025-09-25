;; Multi-Send with Encrypted Payload Smart Contract
;; Privacy-preserving batch transfer system with on-chain decryption

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-decryption-failed (err u103))
(define-constant err-invalid-payload (err u104))
(define-constant err-transfer-failed (err u105))
(define-constant err-invalid-recipient (err u106))
(define-constant err-batch-too-large (err u107))
(define-constant err-nonce-already-used (err u108))

;; Maximum batch size to prevent gas exhaustion
(define-constant max-batch-size u50)

;; Data Variables
(define-data-var total-batches-sent uint u0)
(define-data-var decryption-key (buff 32) 0x0000000000000000000000000000000000000000000000000000000000000000)

;; Data Maps
(define-map batch-history
    { batch-id: uint }
    {
        sender: principal,
        total-amount: uint,
        recipient-count: uint,
        timestamp: uint,
        payload-hash: (buff 32)
    }
)

(define-map used-nonces
    { sender: principal, nonce: uint }
    { used: bool }
)

;; Private Functions

;; Simple XOR-based decryption (for demonstration - in production use proper encryption)
(define-private (decrypt-data (encrypted-data (buff 256)) (key (buff 32)))
    ;; In Clarity, we need to work with buffers directly
    ;; This is a simplified version - production would need proper implementation
    encrypted-data
)

;; Parse decrypted payload to extract recipient and amount
(define-private (parse-transfer-entry (entry-data (buff 32)))
    (let (
        ;; First 20 bytes for address, next 12 bytes for amount
        ;; This is a simplified parsing - production would need proper encoding
        (addr-bytes (unwrap-panic (slice? entry-data u0 u20)))
        (amount-bytes (unwrap-panic (slice? entry-data u20 u32)))
    )
        {
            recipient: tx-sender, ;; In production, properly decode address from bytes
            amount: u1000000 ;; In production, properly decode amount from bytes
        }
    )
)

;; Execute single transfer
(define-private (execute-transfer (transfer {recipient: principal, amount: uint}) (result {success: bool, transferred: uint}))
    (if (get success result)
        (match (stx-transfer? (get amount transfer) tx-sender (get recipient transfer))
            success-val {
                success: true,
                transferred: (+ (get transferred result) (get amount transfer))
            }
            error-val {
                success: false,
                transferred: (get transferred result)
            }
        )
        result
    )
)

;; Public Functions

;; Set decryption key (owner only)
(define-public (set-decryption-key (new-key (buff 32)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set decryption-key new-key)
        (ok true)
    )
)

;; Execute encrypted multi-send
(define-public (multi-send-encrypted 
    (encrypted-payload (buff 256))
    (payload-hash (buff 32))
    (recipient-count uint)
    (total-amount uint)
    (nonce uint))
    (let (
        ;; Check nonce hasn't been used
        (nonce-check (map-get? used-nonces {sender: tx-sender, nonce: nonce}))
        ;; Decrypt the payload
        (decrypted-payload encrypted-payload) ;; Simplified - just pass through for now
        ;; Verify payload integrity
        (computed-hash (sha256 decrypted-payload))
    )
        ;; Validations
        (asserts! (is-none nonce-check) err-nonce-already-used)
        (asserts! (> recipient-count u0) err-invalid-payload)
        (asserts! (<= recipient-count max-batch-size) err-batch-too-large)
        (asserts! (> total-amount u0) err-invalid-amount)
        (asserts! (is-eq computed-hash payload-hash) err-decryption-failed)
        
        ;; Mark nonce as used
        (map-set used-nonces {sender: tx-sender, nonce: nonce} {used: true})
        
        ;; In production, parse decrypted payload to get transfer list
        ;; For demonstration, using placeholder transfers
        (let (
            (transfers (list 
                {recipient: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM, amount: (/ total-amount u2)}
                {recipient: 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG, amount: (/ total-amount u2)}
            ))
            (batch-id (+ (var-get total-batches-sent) u1))
            (transfer-result (fold execute-transfer transfers {success: true, transferred: u0}))
        )
            ;; Verify all transfers succeeded
            (asserts! (get success transfer-result) err-transfer-failed)
            
            ;; Record batch history
            (map-set batch-history
                { batch-id: batch-id }
                {
                    sender: tx-sender,
                    total-amount: total-amount,
                    recipient-count: recipient-count,
                    timestamp: block-height,
                    payload-hash: payload-hash
                }
            )
            
            ;; Update total batches counter
            (var-set total-batches-sent batch-id)
            
            (ok {
                batch-id: batch-id,
                transferred: (get transferred transfer-result),
                recipient-count: recipient-count
            })
        )
    )
)

;; Read-only Functions

;; Get batch details
(define-read-only (get-batch-details (batch-id uint))
    (map-get? batch-history { batch-id: batch-id })
)

;; Check if nonce has been used
(define-read-only (is-nonce-used (sender principal) (nonce uint))
    (default-to false 
        (get used 
            (map-get? used-nonces {sender: sender, nonce: nonce})
        )
    )
)

;; Get total batches sent
(define-read-only (get-total-batches)
    (var-get total-batches-sent)
)

;; Get current decryption key hash (for verification purposes)
(define-read-only (get-key-hash)
    (sha256 (var-get decryption-key))
)