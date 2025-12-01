; Debug whitespace skipping

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

; Skip whitespace
(define (skip-whitespace lex)
  (print 10)
  (print (cdr lex))
  (if (lexer-eof? lex)
      (begin
        (print 11)
        lex)
      (let ((ch (lexer-peek lex)))
        (print 12)
        (print ch)
        (print (char-whitespace? ch))
        (if (char-whitespace? ch)
            (skip-whitespace (lexer-advance lex))
            lex))))

; Test
(define test-src "(+ 1)")
(define lex (make-lexer test-src))
(print 1)
(define lex2 (skip-whitespace lex))
(print 2)
(print (cdr lex2))
