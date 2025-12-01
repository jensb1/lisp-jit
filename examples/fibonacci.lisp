; Fibonacci sequence

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(print "Fibonacci sequence:")
(print (fib 0))   ; 0
(print (fib 1))   ; 1
(print (fib 2))   ; 1
(print (fib 3))   ; 2
(print (fib 4))   ; 3
(print (fib 5))   ; 5
(print (fib 10))  ; 55
(print (fib 15))  ; 610
