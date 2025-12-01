; Test nested define (does it work?)
(define (outer x)
  (define (inner y)
    (+ x y))
  (inner 10))

(print (outer 5))
