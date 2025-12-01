; Test new primitives

; Test cons, car, cdr
(define pair (cons 1 2))
(print (car pair))
(print (cdr pair))

; Test list
(define mylist (list 10 20 30))
(print (car mylist))
(print (car (cdr mylist)))
(print (car (cdr (cdr mylist))))

; Test null?
(print (null? (list)))
(print (null? mylist))
