; Debug car issue

(define (test-car-fn pair)
  (print 10)
  (print (car pair))
  (print 11)
  (print (cdr pair))
  (print 12))

; Test cons with integers
(define p1 (cons 42 99))
(print 1)
(print (car p1))
(print (cdr p1))

; Test passing to function
(print 2)
(test-car-fn p1)

; Test cons with string
(define p2 (cons "ab" 0))
(print 3)
(print (string-length (car p2)))

; Test passing string cons to function
(define (test-str-fn pair)
  (print 20)
  (let ((s (car pair)))
    (print 21)
    (print (string-length s))
    (print 22)))

(print 4)
(test-str-fn p2)
