; Debug with detailed lexer-eof trace

(define (make-lexer source)
  (cons source 0))

(define (lexer-eof? lex)
  (print 50)
  (print (cdr lex))
  (let ((src (car lex)))
    (print 51)
    (print (string-length src))
    (>= (cdr lex) (string-length src))))

(define (read-token lex)
  (print 60)
  (let ((eof (lexer-eof? lex)))
    (print 61)
    (print eof)
    (if eof
        (list)
        (cons 1 (cons (car lex) (+ (cdr lex) 1))))))

(define (tokenize-helper lex tokens)
  (print 70)
  (let ((result (read-token lex)))
    (print 71)
    (print (null? result))
    (if (null? result)
        tokens
        (tokenize-helper (cdr result) (cons (car result) tokens)))))

(define test-src "ab")
(define lex (make-lexer test-src))
(print 100)
(define tokens (tokenize-helper lex (list)))
(print 200)
