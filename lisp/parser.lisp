; Lisp Parser - converts tokens to AST
; AST is represented as:
; - Numbers: just the number
; - Symbols: (3 . symbol-name-string)
; - Strings: (4 . string-value)
; - Lists: a Lisp list of AST nodes

; Include lexer functions (in real bootstrap, would be a module)
; For now, just the token accessors

(define (token-type tok) (car tok))
(define (token-value tok) (cdr tok))

; Parser state: (tokens . current-index)
(define (make-parser tokens)
  (cons tokens 0))

(define (parser-tokens p) (car p))
(define (parser-pos p) (cdr p))

(define (parser-set-pos p pos)
  (cons (car p) pos))

; Get nth element of list
(define (list-ref-helper lst n)
  (if (= n 0)
      (car lst)
      (list-ref-helper (cdr lst) (- n 1))))

(define (list-ref lst n)
  (list-ref-helper lst n))

; List length
(define (list-length-helper lst acc)
  (if (null? lst)
      acc
      (list-length-helper (cdr lst) (+ acc 1))))

(define (list-length lst)
  (list-length-helper lst 0))

; Parser EOF?
(define (parser-eof? p)
  (>= (parser-pos p) (list-length (parser-tokens p))))

; Current token
(define (parser-current p)
  (if (parser-eof? p)
      (list)
      (list-ref (parser-tokens p) (parser-pos p))))

; Advance parser
(define (parser-advance p)
  (parser-set-pos p (+ (parser-pos p) 1)))

; Reverse list
(define (reverse-helper lst acc)
  (if (null? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(define (reverse lst)
  (reverse-helper lst (list)))

; Parse one expression, returns (expr . new-parser)
(define (parse-expr p)
  (if (parser-eof? p)
      (list)  ; error: unexpected EOF
      (let ((tok (parser-current p)))
        (let ((typ (token-type tok))
              (val (token-value tok)))
          (if (= typ 0)  ; lparen
              (parse-list (parser-advance p))
              (if (= typ 1)  ; rparen - error
                  (list)
                  (if (= typ 2)  ; number
                      (cons val (parser-advance p))
                      (if (= typ 3)  ; symbol
                          (cons (cons 3 val) (parser-advance p))
                          (if (= typ 4)  ; string
                              (cons (cons 4 val) (parser-advance p))
                              (list))))))))))  ; unknown token type

; Parse list contents until ), returns (list-of-exprs . new-parser)
(define (parse-list-helper p elements)
  (if (parser-eof? p)
      (list)  ; error: unclosed list
      (let ((tok (parser-current p)))
        (if (= (token-type tok) 1)  ; rparen
            (cons (reverse elements) (parser-advance p))
            (let ((result (parse-expr p)))
              (if (null? result)
                  (list)  ; error from parse-expr
                  (parse-list-helper (cdr result) (cons (car result) elements))))))))

(define (parse-list p)
  (parse-list-helper p (list)))

; Parse all expressions from token list
(define (parse-all-helper p exprs)
  (if (parser-eof? p)
      (reverse exprs)
      (let ((result (parse-expr p)))
        (if (null? result)
            (reverse exprs)  ; stop on error
            (parse-all-helper (cdr result) (cons (car result) exprs))))))

(define (parse tokens)
  (parse-all-helper (make-parser tokens) (list)))

; ===== Test =====

; Simple tokenize function for testing
; (reusing code from lexer.lisp would be better)

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

(define (skip-whitespace lex)
  (if (lexer-eof? lex)
      lex
      (if (char-whitespace? (lexer-peek lex))
          (skip-whitespace (lexer-advance lex))
          lex)))

(define (symbol-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (= c 45) (= c 43) (= c 42) (= c 47)
      (= c 60) (= c 62) (= c 61)
      (= c 95) (= c 33) (= c 63)))

(define (read-digits-helper lex acc)
  (if (lexer-eof? lex)
      (cons acc lex)
      (let ((ch (lexer-peek lex)))
        (if (char-numeric? ch)
            (read-digits-helper (lexer-advance lex) (+ (* acc 10) (- ch 48)))
            (cons acc lex)))))

(define (read-number lex)
  (read-digits-helper lex 0))

(define (build-string-helper chars acc)
  (if (null? chars)
      acc
      (build-string-helper (cdr chars) (string-append acc (make-char-string (car chars))))))

(define (chars-to-string chars)
  (build-string-helper chars ""))

(define (read-symbol-chars lex chars)
  (if (lexer-eof? lex)
      (cons chars lex)
      (let ((ch (lexer-peek lex)))
        (if (symbol-char? ch)
            (read-symbol-chars (lexer-advance lex) (cons ch chars))
            (cons chars lex)))))

(define (read-symbol lex)
  (let ((result (read-symbol-chars lex (list))))
    (cons (chars-to-string (reverse (car result))) (cdr result))))

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

(define (tokenize-helper lex tokens)
  (let ((result (read-token lex)))
    (if (null? result)
        (reverse tokens)
        (tokenize-helper (cdr result) (cons (car result) tokens)))))

(define (tokenize source)
  (tokenize-helper (make-lexer source) (list)))

; Test parsing
(define test-src "(+ 1 2)")
(define tokens (tokenize test-src))
(define ast (parse tokens))

; Print AST structure
(define (print-ast node)
  (if (null? node)
      (print 0)
      (if (pair? node)
          (if (= (car node) 3)  ; symbol
              (begin
                (print 3)  ; symbol marker
                0)
              (if (= (car node) 4)  ; string
                  (begin
                    (print 4)  ; string marker
                    0)
                  ; it's a list
                  (print-ast-list node)))
          ; it's a number
          (print node))))

(define (print-ast-list lst)
  (print 100)  ; list start marker
  (print-ast-list-helper lst)
  (print 101))  ; list end marker

(define (print-ast-list-helper lst)
  (if (null? lst)
      0
      (begin
        (print-ast (car lst))
        (print-ast-list-helper (cdr lst)))))

; Print number of top-level expressions
(print (list-length ast))

; Print first expression
(print-ast (car ast))
