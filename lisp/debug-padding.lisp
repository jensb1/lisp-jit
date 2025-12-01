; Debug padding issue

(define (mod a b) (- a (* (/ a b) b)))

; Test mod
(print (mod 8 16))      ; should be 8
(print (mod 16 16))     ; should be 0
(print (mod 340 16384)) ; should be 340

; Test what happens with header size
(define header-size 16384)
(define test-pos 340)
(define rem (mod test-pos header-size))
(print rem)  ; should be 340
(print (- header-size rem))  ; should be 16044

; Full test
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

(define (emit-zeros-m m n)
  (if (<= n 0) 0
      (begin (emit-byte-m m 0) (emit-zeros-m m (- n 1)))))

(define (emit-pad-m m align)
  (let ((pos (macho-pos m)))
    (let ((rem (mod pos align)))
      (print 777)  ; marker
      (print pos)
      (print rem)
      (if (= rem 0) 0
          (begin
            (print (- align rem))  ; how many zeros to add
            (emit-zeros-m m (- align rem)))))))

(define m (make-macho))

; Simulate writing 340 bytes of header
(macho-set-pos! m 340)
(print (macho-pos m))

; Now pad to 16384
(emit-pad-m m 16384)
(print (macho-pos m))  ; should be 16384
