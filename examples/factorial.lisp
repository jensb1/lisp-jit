; Factorial using recursion

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print "Factorials:")
(print (factorial 1))   ; 1
(print (factorial 5))   ; 120
(print (factorial 10))  ; 3628800
