; Test let
(print 1)
(let ((x 42))
  (print x))
(print 2)

; Test let with function call
(define (double n)
  (* n 2))

(print 3)
(let ((y (double 5)))
  (print y))
(print 4)
