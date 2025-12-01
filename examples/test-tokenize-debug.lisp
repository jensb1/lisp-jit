; Debug tokenization

(define (make-token type value)
  (cons type value))

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

; Simple read-token: just read first char
(define (read-token lex)
  (if (lexer-eof? lex)
      (list)  ; EOF
      (let ((ch (lexer-peek lex)))
        (cons (make-token 0 ch) (lexer-advance lex)))))

; Tokenize: collect all tokens
(define (tokenize-helper lex tokens)
  (print (lexer-pos lex))  ; debug
  (let ((result (read-token lex)))
    (if (null? result)
        tokens
        (tokenize-helper (cdr result) (cons (car result) tokens)))))

(define test-src "abc")
(define tokens (tokenize-helper (make-lexer test-src) (list)))

(print 999)  ; marker
