; Debug lexer-eof inside function

(define (make-lexer source)
  (cons source 0))

(define (lexer-eof? lex)
  (print 101)
  (print (cdr lex))  ; position
  (print (string-length (car lex)))  ; source length
  (>= (cdr lex) (string-length (car lex))))

(define (test-fn lex)
  (print 200)
  (print (lexer-eof? lex))
  (print 201))

(define test-src "ab")
(define lex (make-lexer test-src))

; Test at top level
(print 300)
(print (lexer-eof? lex))
(print 301)

; Test inside function
(print 400)
(test-fn lex)
(print 401)
