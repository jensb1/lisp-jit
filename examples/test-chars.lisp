; Test chars-to-string

; Reverse a list
(define (reverse lst)
  (define (rev-helper lst acc)
    (if (null? lst)
        acc
        (rev-helper (cdr lst) (cons (car lst) acc))))
  (rev-helper lst (list)))

; Convert list of char codes to string
(define (chars-to-string chars)
  (define (build chars acc)
    (if (null? chars)
        acc
        (build (cdr chars)
               (string-append acc (make-char-string (car chars))))))
  (build chars ""))

; Test
(define chars (list 97 98 99))  ; a, b, c
(define str (chars-to-string chars))
(print (string-length str))
(print (string-ref str 0))
(print (string-ref str 1))
(print (string-ref str 2))
