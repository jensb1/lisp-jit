; Test large vector in assembler

(define s (make-vector 6))
(define buf (make-vector 131072))
(vector-set! s 0 buf)
(vector-set! s 1 0)

(print (vector-length buf))
(print (vector-length (vector-ref s 0)))

; Write some bytes
(vector-set! (vector-ref s 0) 0 207)
(vector-set! (vector-ref s 0) 1 250)
(vector-set! s 1 2)

; Read back
(print (vector-ref s 1))  ; pos = 2
(print (vector-ref (vector-ref s 0) 0))  ; should be 207
(print (vector-ref (vector-ref s 0) 1))  ; should be 250
