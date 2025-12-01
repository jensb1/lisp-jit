; Lisp Lexer - tokenizes source code into tokens
; Each token is (type . value) where type is one of:
; 0 = lparen, 1 = rparen, 2 = number, 3 = symbol, 4 = string
; NOTE: No nested defines - they don't work in this JIT yet

; Make a token
(define (make-token type value)
  (cons type value))

(define (token-type tok)
  (car tok))

(define (token-value tok)
  (cdr tok))

; Lexer state: (source . position)
(define (make-lexer source)
  (cons source 0))

(define (lexer-source lex)
  (car lex))

(define (lexer-pos lex)
  (cdr lex))

(define (lexer-set-pos lex pos)
  (cons (car lex) pos))

(define (lexer-eof? lex)
  (>= (lexer-pos lex) (string-length (lexer-source lex))))

(define (lexer-peek lex)
  (if (lexer-eof? lex)
      0
      (string-ref (lexer-source lex) (lexer-pos lex))))

(define (lexer-advance lex)
  (lexer-set-pos lex (+ (lexer-pos lex) 1)))

; Skip whitespace
(define (skip-whitespace lex)
  (if (lexer-eof? lex)
      lex
      (if (char-whitespace? (lexer-peek lex))
          (skip-whitespace (lexer-advance lex))
          lex)))

; Skip comment (from ; to end of line)
(define (skip-comment lex)
  (if (lexer-eof? lex)
      lex
      (let ((ch (lexer-peek lex)))
        (if (= ch 10)
            (lexer-advance lex)
            (skip-comment (lexer-advance lex))))))

; Skip whitespace and comments
(define (skip-ws-comments lex)
  (let ((lex2 (skip-whitespace lex)))
    (if (lexer-eof? lex2)
        lex2
        (if (= (lexer-peek lex2) 59)
            (skip-ws-comments (skip-comment lex2))
            lex2))))

; Check if character is symbol char
(define (symbol-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (= c 45)
      (= c 43)
      (= c 42)
      (= c 47)
      (= c 60)
      (= c 62)
      (= c 61)
      (= c 95)
      (= c 33)
      (= c 63)))

; Reverse helper
(define (reverse-helper lst acc)
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-helper lst (list)))

; Build string from char list helper
(define (build-string-helper chars acc)
  (if (null? chars)
      acc
      (build-string-helper (cdr chars)
                           (string-append acc (make-char-string (car chars))))))

(define (chars-to-string chars)
  (build-string-helper chars ""))

; Read digits helper (for numbers)
(define (read-digits-helper lex acc)
  (if (lexer-eof? lex)
      (cons acc lex)
      (let ((ch (lexer-peek lex)))
        (if (char-numeric? ch)
            (read-digits-helper (lexer-advance lex)
                               (+ (* acc 10) (- ch 48)))
            (cons acc lex)))))

(define (read-number lex)
  (let ((neg (= (lexer-peek lex) 45)))
    (let ((lex2 (if neg (lexer-advance lex) lex)))
      (let ((result (read-digits-helper lex2 0)))
        (cons (if neg (- 0 (car result)) (car result))
              (cdr result))))))

; Read symbol chars helper
(define (read-symbol-chars lex chars)
  (if (lexer-eof? lex)
      (cons chars lex)
      (let ((ch (lexer-peek lex)))
        (if (symbol-char? ch)
            (read-symbol-chars (lexer-advance lex) (cons ch chars))
            (cons chars lex)))))

(define (read-symbol lex)
  (let ((result (read-symbol-chars lex (list))))
    (cons (chars-to-string (reverse (car result)))
          (cdr result))))

; Read string literal chars helper
(define (read-string-chars lex chars)
  (if (lexer-eof? lex)
      (cons chars lex)
      (let ((ch (lexer-peek lex)))
        (if (= ch 34)
            (cons chars (lexer-advance lex))
            (if (= ch 92)
                (let ((lex2 (lexer-advance lex)))
                  (if (lexer-eof? lex2)
                      (cons chars lex2)
                      (let ((esc (lexer-peek lex2)))
                        (read-string-chars
                          (lexer-advance lex2)
                          (cons (if (= esc 110) 10
                                    (if (= esc 116) 9 esc))
                                chars)))))
                (read-string-chars (lexer-advance lex) (cons ch chars)))))))

(define (read-string-literal lex)
  (let ((result (read-string-chars (lexer-advance lex) (list))))
    (cons (chars-to-string (reverse (car result)))
          (cdr result))))

; Check if starts with digit or is negative number
(define (starts-number? lex)
  (let ((ch (lexer-peek lex)))
    (or (char-numeric? ch)
        (and (= ch 45)
             (let ((lex2 (lexer-advance lex)))
               (and (not (lexer-eof? lex2))
                    (char-numeric? (lexer-peek lex2))))))))

; Read one token
(define (read-token lex)
  (let ((lex2 (skip-ws-comments lex)))
    (if (lexer-eof? lex2)
        (list)
        (let ((ch (lexer-peek lex2)))
          (if (= ch 40)
              (cons (make-token 0 "(") (lexer-advance lex2))
              (if (= ch 41)
                  (cons (make-token 1 ")") (lexer-advance lex2))
                  (if (= ch 34)
                      (let ((result (read-string-literal lex2)))
                        (cons (make-token 4 (car result)) (cdr result)))
                      (if (starts-number? lex2)
                          (let ((result (read-number lex2)))
                            (cons (make-token 2 (car result)) (cdr result)))
                          (let ((result (read-symbol lex2)))
                            (cons (make-token 3 (car result)) (cdr result)))))))))))

; Tokenize helper
(define (tokenize-helper lex tokens)
  (let ((result (read-token lex)))
    (if (null? result)
        (reverse tokens)
        (tokenize-helper (cdr result) (cons (car result) tokens)))))

(define (tokenize source)
  (tokenize-helper (make-lexer source) (list)))

; ===== Test the lexer =====
(define test-source "(+ 1 2)")
(define test-tokens (tokenize test-source))

; Count tokens
(define (list-length-helper lst acc)
  (if (null? lst)
      acc
      (list-length-helper (cdr lst) (+ acc 1))))

(define (list-length lst)
  (list-length-helper lst 0))

(print (list-length test-tokens))

; Print token types
(define (print-token-types toks)
  (if (null? toks)
      0
      (begin
        (print (token-type (car toks)))
        (print-token-types (cdr toks)))))

(print-token-types test-tokens)
