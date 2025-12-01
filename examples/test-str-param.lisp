; Is it the let or the parameter?

(define (test-no-let pair)
  (print (string-length (car pair))))

(define (test-with-let pair)
  (let ((s (car pair)))
    (print (string-length s))))

(define p (cons "ab" 0))

(print 1)
(print (string-length (car p)))  ; at top level

(print 2)
(test-no-let p)

(print 3)
(test-with-let p)
