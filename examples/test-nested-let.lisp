; Test nested let
(define (test-nested x)
  (let ((a (+ x 1)))
    (print a)
    (let ((b (+ a 1)))
      (print b)
      (+ a b))))

(print (test-nested 10))
