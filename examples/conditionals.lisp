; Conditionals and comparisons

(print "Conditionals:")

; Simple if
(print (if (> 10 5) 1 0))   ; 1 (true)
(print (if (< 10 5) 1 0))   ; 0 (false)

; Equality
(print (if (= 42 42) 100 200))  ; 100

; Greater/less or equal
(print (if (>= 10 10) 1 0))  ; 1
(print (if (<= 5 10) 1 0))   ; 1

; Nested if
(define x 15)
(print (if (> x 20)
           1
           (if (> x 10)
               2
               3)))  ; Should print 2
