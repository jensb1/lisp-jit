; Debug recursion

(define (make-lexer source)
  (cons source 0))

(define (lexer-eof? lex)
  (>= (cdr lex) (string-length (car lex))))

(define (lexer-peek lex)
  (string-ref (car lex) (cdr lex)))

(define (lexer-advance lex)
  (cons (car lex) (+ (cdr lex) 1)))

(define (make-token ch)
  (cons 0 ch))

; Simple read-token
(define (read-token lex)
  (if (lexer-eof? lex)
      (list)
      (cons (make-token (lexer-peek lex))
            (lexer-advance lex))))

; Test read-token
(define test-src "ab")
(define lex0 (make-lexer test-src))

(define result (read-token lex0))
(print (null? result))  ; should be 0 (not null)

(define tok (car result))
(define new-lex (cdr result))
(print (cdr tok))  ; char code of token, should be 97
(print (cdr new-lex))  ; new position, should be 1

; Try second token
(define result2 (read-token new-lex))
(print (null? result2))  ; should be 0
(define tok2 (car result2))
(print (cdr tok2))  ; should be 98

; Try third (should be EOF)
(define new-lex2 (cdr result2))
(define result3 (read-token new-lex2))
(print (null? result3))  ; should be 1 (EOF)
