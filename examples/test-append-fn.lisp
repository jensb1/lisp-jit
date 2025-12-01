; Test string-append in a function
(define (build-string chars acc)
  (if (null? chars)
      acc
      (build-string (cdr chars)
                   (string-append acc (make-char-string (car chars))))))

(define chars (list 97 98 99))
(define result (build-string chars ""))
(print (string-length result))

; Also test that we can call string-append from a function
(define (test-append)
  (string-append "a" "b"))

(define s2 (test-append))
(print (string-length s2))
