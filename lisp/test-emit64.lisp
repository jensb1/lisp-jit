; Test 64-bit emission

(define base-vmaddr 4294967296)  ; 0x100000000

(print base-vmaddr)
(print (bitand base-vmaddr 4294967295))   ; Low 32 bits - should be 0
(print (asr base-vmaddr 32))              ; High 32 bits - should be 1
(print (bitand (asr base-vmaddr 32) 4294967295))  ; Should be 1

; The issue might be asr with large numbers
(define big 4294967296)
(print (asr big 32))  ; Should be 1
