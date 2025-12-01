; Debug tokenization step 2

(define (make-lexer source)
  (cons source 0))

(define (lexer-eof? lex)
  (>= (cdr lex) (string-length (car lex))))

(define (lexer-peek lex)
  (string-ref (car lex) (cdr lex)))

(define (lexer-advance lex)
  (cons (car lex) (+ (cdr lex) 1)))

; Test lex operations
(define test-src "ab")
(define lex0 (make-lexer test-src))

(print (lexer-eof? lex0))  ; should be 0 (false)
(print (lexer-peek lex0))  ; should be 97 ('a')

(define lex1 (lexer-advance lex0))
(print (cdr lex1))  ; should be 1
(print (lexer-eof? lex1))  ; should be 0 (false)
(print (lexer-peek lex1))  ; should be 98 ('b')

(define lex2 (lexer-advance lex1))
(print (cdr lex2))  ; should be 2
(print (lexer-eof? lex2))  ; should be 1 (true)
