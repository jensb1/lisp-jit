; Test pair?
(print (pair? (cons 1 2)))  ; 1
(print (pair? 42))          ; 0
(print (pair? (list)))      ; 0 (nil is not a pair)
(print (pair? (list 1 2)))  ; 1
