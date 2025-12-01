; Debug nested let more carefully
(define (test-nested x)
  (print 1)
  (print x)
  (let ((a (+ x 1)))
    (print 2)
    (print a)
    (print x)  ; can we still access x?
    (let ((b 99))
      (print 3)
      (print a)  ; can we access outer let's a?
      (print x)  ; can we access param x?
      a)))

(print (test-nested 10))
