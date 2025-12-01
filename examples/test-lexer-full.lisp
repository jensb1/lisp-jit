; Test full lexer with "(+ 1 2)"

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

; Is symbol char?
(define (symbol-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (= c 45)   ; -
      (= c 43)   ; +
      (= c 42)   ; *
      (= c 47)   ; /
      (= c 60)   ; <
      (= c 62)   ; >
      (= c 61)   ; =
      (= c 95)   ; _
      (= c 33)   ; !
      (= c 63))) ; ?

; Read digits
(define (read-digits-helper lex acc)
  (if (lexer-eof? lex)
      (cons acc lex)
      (let ((ch (lexer-peek lex)))
        (if (char-numeric? ch)
            (read-digits-helper (lexer-advance lex)
                               (+ (* acc 10) (- ch 48)))
            (cons acc lex)))))

; Read number
(define (read-number lex)
  (read-digits-helper lex 0))

; Reverse list
(define (reverse-helper lst acc)
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-helper lst (list)))

; Build string from chars
(define (build-string-helper chars acc)
  (if (null? chars)
      acc
      (build-string-helper (cdr chars)
                           (string-append acc (make-char-string (car chars))))))

(define (chars-to-string chars)
  (build-string-helper chars ""))

; Read symbol chars
(define (read-symbol-chars lex chars)
  (if (lexer-eof? lex)
      (cons chars lex)
      (let ((ch (lexer-peek lex)))
        (if (symbol-char? ch)
            (read-symbol-chars (lexer-advance lex) (cons ch chars))
            (cons chars lex)))))

; Read symbol
(define (read-symbol lex)
  (let ((result (read-symbol-chars lex (list))))
    (cons (chars-to-string (reverse (car result)))
          (cdr result))))

; Read one token (0=lparen, 1=rparen, 2=number, 3=symbol)
(define (read-token lex)
  (let ((lex2 (skip-whitespace lex)))
    (if (lexer-eof? lex2)
        (list)
        (let ((ch (lexer-peek lex2)))
          (if (= ch 40)
              (cons (make-token 0 "(") (lexer-advance lex2))
              (if (= ch 41)
                  (cons (make-token 1 ")") (lexer-advance lex2))
                  (if (char-numeric? ch)
                      (let ((result (read-number lex2)))
                        (cons (make-token 2 (car result)) (cdr result)))
                      (let ((result (read-symbol lex2)))
                        (cons (make-token 3 (car result)) (cdr result))))))))))

; Tokenize
(define (tokenize-helper lex tokens)
  (let ((result (read-token lex)))
    (if (null? result)
        (reverse tokens)
        (tokenize-helper (cdr result) (cons (car result) tokens)))))

(define (tokenize source)
  (tokenize-helper (make-lexer source) (list)))

; Helpers
(define (list-length-helper lst acc)
  (if (null? lst)
      acc
      (list-length-helper (cdr lst) (+ acc 1))))

(define (list-length lst)
  (list-length-helper lst 0))

(define (token-type tok)
  (car tok))

(define (token-value tok)
  (cdr tok))

(define (print-token-types toks)
  (if (null? toks)
      0
      (begin
        (print (token-type (car toks)))
        (print-token-types (cdr toks)))))

; Test
(define test-source "(+ 1 2)")
(define tokens (tokenize test-source))
(print (list-length tokens))
(print-token-types tokens)
