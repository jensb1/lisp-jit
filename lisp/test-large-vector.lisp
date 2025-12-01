; Test large vector

(define big (make-vector 1048576))
(print (vector-length big))

; Write some values
(vector-set! big 0 207)
(vector-set! big 1 250)
(vector-set! big 100000 42)
(vector-set! big 500000 99)

; Read them back
(print (vector-ref big 0))
(print (vector-ref big 1))
(print (vector-ref big 100000))
(print (vector-ref big 500000))
