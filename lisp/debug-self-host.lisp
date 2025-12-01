; Debug self-hosting compiler - trace Mach-O generation

; Minimal Mach-O test

(define MH-MAGIC-64 4277009103)
(define CPU-TYPE-ARM64 16777228)

(define (make-macho)
  (let ((m (make-vector 3)))
    (vector-set! m 0 (make-vector 1000))
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

(define (output-binary m)
  (let ((buf (macho-buf m)) (len (macho-pos m)))
    (print len)
    (output-bin-h buf 0 len)))

(define (output-bin-h buf idx len)
  (if (>= idx len) 0
      (begin
        (print (vector-ref buf idx))
        (output-bin-h buf (+ idx 1) len))))

; Create macho and emit just the header start
(define m (make-macho))

(print 999)  ; marker
(print (macho-pos m))  ; should be 0

(emit32-m m MH-MAGIC-64)
(print (macho-pos m))  ; should be 4

(emit32-m m CPU-TYPE-ARM64)
(print (macho-pos m))  ; should be 8

(print 888)  ; marker

; Output what we have
(output-binary m)
