; Test string primitives

(define s "hello")
(print (string-length s))
(print (string-ref s 0))
(print (string-ref s 1))

; Test substring
(define sub (substring "hello world" 0 5))
(print (string-length sub))

; Test string-append
(define combined (string-append "foo" "bar"))
(print (string-length combined))

; Test string=?
(print (string=? "abc" "abc"))
(print (string=? "abc" "def"))
