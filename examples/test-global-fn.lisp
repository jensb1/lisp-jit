; Test global variable access from function
(define test-source "(+ 1 2)")

(define (get-length)
  (string-length test-source))

(print (string-length test-source))
(print (get-length))
