; Debug nested define
(define (outer x)
  (define (inner y)
    (print x)
    (print y)
    (+ x y))
  (inner 10))

(print (outer 5))
