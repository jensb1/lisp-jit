; Debug full header generation

(define MH-MAGIC-64 4277009103)
(define CPU-TYPE-ARM64 16777228)
(define MH-EXECUTE 2)
(define MH-NOUNDEFS 1)
(define MH-DYLDLINK 4)
(define MH-PIE 2097152)
(define LC-SEGMENT-64 25)
(define LC-BUILD-VERSION 50)
(define LC-MAIN 2147483688)
(define VM-PROT-READ 1)
(define VM-PROT-EXECUTE 4)
(define S-ATTR-PURE-INSTRUCTIONS 2147483648)
(define S-ATTR-SOME-INSTRUCTIONS 1024)
(define PLATFORM-MACOS 1)

(define (mod a b) (- a (* (/ a b) b)))

(define (make-macho)
  (let ((m (make-vector 3)))
    (vector-set! m 0 (make-vector 20000))
    (vector-set! m 1 0)
    m))

(define (macho-buf m) (vector-ref m 0))
(define (macho-pos m) (vector-ref m 1))
(define (macho-set-pos! m p) (vector-set! m 1 p))

(define (emit-byte-m m b)
  (vector-set! (macho-buf m) (macho-pos m) (bitand b 255))
  (macho-set-pos! m (+ (macho-pos m) 1)))

(define (emit32-m m i)
  (emit-byte-m m (bitand i 255))
  (emit-byte-m m (bitand (asr i 8) 255))
  (emit-byte-m m (bitand (asr i 16) 255))
  (emit-byte-m m (bitand (asr i 24) 255)))

(define (emit64-m m i)
  (emit32-m m (bitand i 4294967295))
  (emit32-m m (bitand (asr i 32) 4294967295)))

(define (emit-str-m m str len)
  (emit-str-m-h m str 0 (string-length str) len))

(define (emit-str-m-h m str idx slen padlen)
  (if (>= idx padlen) 0
      (begin
        (if (< idx slen)
            (emit-byte-m m (string-ref str idx))
            (emit-byte-m m 0))
        (emit-str-m-h m str (+ idx 1) slen padlen))))

(define (emit-zeros-m m n)
  (if (<= n 0) 0
      (begin (emit-byte-m m 0) (emit-zeros-m m (- n 1)))))

(define (emit-pad-m m align)
  (let ((rem (mod (macho-pos m) align)))
    (if (= rem 0) 0 (emit-zeros-m m (- align rem)))))

; Simplified header - just first part
(define (gen-simple-header m code-size)
  (let ((header-size 4096)
        (base-vmaddr 4294967296)
        (text-vmaddr 4294983680))
    ; Mach-O header
    (emit32-m m MH-MAGIC-64)
    (emit32-m m CPU-TYPE-ARM64)
    (emit32-m m 0)
    (emit32-m m MH-EXECUTE)
    (emit32-m m 4)
    (emit32-m m 312)
    (emit32-m m (bitor MH-NOUNDEFS (bitor MH-DYLDLINK MH-PIE)))
    (emit32-m m 0)))

(define m (make-macho))

(print 111)  ; marker
(print (macho-pos m))

(gen-simple-header m 100)

(print 222)  ; marker
(print (macho-pos m))  ; should be 32

; Check first 8 bytes
(print 333)  ; marker
(print (vector-ref (macho-buf m) 0))  ; 207
(print (vector-ref (macho-buf m) 1))  ; 250
(print (vector-ref (macho-buf m) 2))  ; 237
(print (vector-ref (macho-buf m) 3))  ; 254
(print (vector-ref (macho-buf m) 4))  ; 12
(print (vector-ref (macho-buf m) 5))  ; 0
(print (vector-ref (macho-buf m) 6))  ; 0
(print (vector-ref (macho-buf m) 7))  ; 1
