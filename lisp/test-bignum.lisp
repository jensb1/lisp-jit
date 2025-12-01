; Test large numbers

; Try different representations
(print (lsl 1 32))          ; Should be 4294967296
(print (lsl 1 31))          ; Should be 2147483648
(print (* 2147483648 2))    ; Should be 4294967296
(print (+ 2147483648 2147483648))  ; Should be 4294967296
