; Test function call inside nested let

(define (add1 n)
  (+ n 1))

(define (test-fn x)
  (let ((a (add1 x)))
    (print a)
    (let ((b (add1 a)))
      (print b)
      b)))

(print (test-fn 10))
