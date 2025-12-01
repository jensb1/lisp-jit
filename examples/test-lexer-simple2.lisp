; Simplified lexer test - no whitespace handling

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

; Just read single-char tokens
(define (read-token lex)
  (if (lexer-eof? lex)
      (list)
      (let ((ch (lexer-peek lex)))
        (if (= ch 40)  ; (
            (cons (make-token 0 "(") (lexer-advance lex))
            (if (= ch 41)  ; )
                (cons (make-token 1 ")") (lexer-advance lex))
                (cons (make-token 3 ch) (lexer-advance lex)))))))

(define (tokenize-helper lex tokens)
  (let ((result (read-token lex)))
    (if (null? result)
        tokens
        (tokenize-helper (cdr result) (cons (car result) tokens)))))

(define (reverse-helper lst acc)
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-helper lst (list)))

(define (tokenize source)
  (reverse (tokenize-helper (make-lexer source) (list))))

(define (list-length-helper lst acc)
  (if (null? lst)
      acc
      (list-length-helper (cdr lst) (+ acc 1))))

(define (list-length lst)
  (list-length-helper lst 0))

(define (token-type tok)
  (car tok))

(define (print-token-types toks)
  (if (null? toks)
      0
      (begin
        (print (token-type (car toks)))
        (print-token-types (cdr toks)))))

; Test with simple input
(define test-source "(ab)")
(define tokens (tokenize test-source))
(print (list-length tokens))
(print-token-types tokens)
