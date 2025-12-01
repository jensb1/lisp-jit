; Debug the copy operation

(define (make-vec n) (make-vector n))

(define src (make-vec 20))
(vector-set! src 0 207)
(vector-set! src 1 250)
(vector-set! src 2 237)
(vector-set! src 3 254)
(vector-set! src 4 12)

(define dst (make-vec 100))

(define state (make-vec 2))
(vector-set! state 0 dst)
(vector-set! state 1 0)

(define (emit-byte2 b)
  (let ((d (vector-ref state 0))
        (p (vector-ref state 1)))
    (vector-set! d p (bitand b 255))
    (vector-set! state 1 (+ p 1))))

(define (copy-code buf size)
  (copy-h2 buf 0 size))

(define (copy-h2 buf idx size)
  (if (>= idx size) 0
      (begin
        (emit-byte2 (vector-ref buf idx))
        (copy-h2 buf (+ idx 1) size))))

; Copy 5 bytes
(copy-code src 5)

; Check result
(print (vector-ref state 1))  ; Should be 5
(print (vector-ref (vector-ref state 0) 0))  ; Should be 207
(print (vector-ref (vector-ref state 0) 1))  ; Should be 250
(print (vector-ref (vector-ref state 0) 2))  ; Should be 237
(print (vector-ref (vector-ref state 0) 3))  ; Should be 254
(print (vector-ref (vector-ref state 0) 4))  ; Should be 12
