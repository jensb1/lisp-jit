; Test make-char-string
(define s (make-char-string 97))
(print (string-length s))
(print (string-ref s 0))

; Test list
(define lst (list 1 2 3))
(print (car lst))
(print (car (cdr lst)))
(print (null? lst))
(print (null? (list)))
