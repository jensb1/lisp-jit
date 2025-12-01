; Debug recursive tokenization with more prints

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
  (print 888)  ; debug: entering read-token
  (print (cdr lex))  ; position
  (if (lexer-eof? lex)
      (begin
        (print 777)  ; debug: EOF
        (list))
      (begin
        (print 666)  ; debug: got token
        (cons (make-token (lexer-peek lex))
              (lexer-advance lex)))))

; Recursive tokenization
(define (tokenize-helper lex tokens)
  (print 555)  ; debug: entering tokenize-helper
  (let ((result (read-token lex)))
    (print 444)  ; debug: got result
    (print (null? result))  ; is it null?
    (if (null? result)
        tokens
        (tokenize-helper (cdr result) (cons (car result) tokens)))))

; Test
(define test-src "ab")
(define lex0 (make-lexer test-src))
(print 111)
(define tokens (tokenize-helper lex0 (list)))
(print 222)
