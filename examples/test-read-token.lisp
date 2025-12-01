; Debug read-token

(define (make-lexer source)
  (cons source 0))

(define (lexer-eof? lex)
  (>= (cdr lex) (string-length (car lex))))

(define (lexer-peek lex)
  (if (lexer-eof? lex)
      0
      (string-ref (car lex) (cdr lex))))

(define (lexer-advance lex)
  (cons (car lex) (+ (cdr lex) 1)))

(define (make-token type value)
  (cons type value))

; Skip whitespace
(define (skip-whitespace lex)
  (if (lexer-eof? lex)
      lex
      (if (char-whitespace? (lexer-peek lex))
          (skip-whitespace (lexer-advance lex))
          lex)))

; Read one token
(define (read-token lex)
  (print 100)
  (let ((lex2 (skip-whitespace lex)))
    (print 101)
    (print (cdr lex2))
    (if (lexer-eof? lex2)
        (begin
          (print 102)
          (list))
        (let ((ch (lexer-peek lex2)))
          (print 103)
          (print ch)
          (print (= ch 40))
          (if (= ch 40)
              (begin
                (print 104)
                (cons (make-token 0 "(") (lexer-advance lex2)))
              (begin
                (print 105)
                (cons (make-token 3 ch) (lexer-advance lex2))))))))

; Test
(define test-src "(+)")
(define lex (make-lexer test-src))
(print 1)
(define result (read-token lex))
(print 2)
(print (null? result))
(print (car (car result)))  ; token type
