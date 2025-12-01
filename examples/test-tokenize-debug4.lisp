; Debug recursive tokenization

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

(define (read-token lex)
  (if (lexer-eof? lex)
      (list)
      (cons (make-token (lexer-peek lex))
            (lexer-advance lex))))

; Recursive tokenization
(define (tokenize-helper lex tokens)
  (let ((result (read-token lex)))
    (if (null? result)
        tokens
        (tokenize-helper (cdr result) (cons (car result) tokens)))))

; Test
(define test-src "abc")
(define lex0 (make-lexer test-src))
(define tokens (tokenize-helper lex0 (list)))

; Count
(define (list-length lst)
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))))

(print (list-length tokens))
