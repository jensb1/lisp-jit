; Simple lexer test

; Make a token (inline constants)
(define (make-token type value)
  (cons type value))

(define (token-type tok)
  (car tok))

; Lexer state: (source . position)
(define (make-lexer source)
  (cons source 0))

(define (lexer-pos lex)
  (cdr lex))

(define (lexer-eof? lex)
  (>= (lexer-pos lex) (string-length (car lex))))

(define (lexer-peek lex)
  (if (lexer-eof? lex)
      0
      (string-ref (car lex) (lexer-pos lex))))

(define (lexer-advance lex)
  (cons (car lex) (+ (lexer-pos lex) 1)))

; Test basic lexer
(define test-src "abc")
(define lex (make-lexer test-src))

(print (string-length test-src))
(print (lexer-pos lex))
(print (lexer-peek lex))

(define lex2 (lexer-advance lex))
(print (lexer-pos lex2))
(print (lexer-peek lex2))
