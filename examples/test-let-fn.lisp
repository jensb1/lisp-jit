; Test let inside function
(define (test-let-fn)
  (print 1)
  (let ((x 42))
    (print x))
  (print 2))

(test-let-fn)
(print 999)
