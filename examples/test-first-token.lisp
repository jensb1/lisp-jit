; Debug first token

(define (make-lexer source)
  (cons source 0))

(define (lexer-eof? lex)
  (>= (cdr lex) (string-length (car lex))))

(define (lexer-peek lex)
  (if (lexer-eof? lex)
      0
      (string-ref (car lex) (cdr lex))))

(define test-src "(+)")
(print (string-length test-src))
(print (string-ref test-src 0))

(define lex (make-lexer test-src))
(print (lexer-peek lex))
(print (= (lexer-peek lex) 40))
