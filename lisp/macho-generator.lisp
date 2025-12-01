; Mach-O Executable Generator for ARM64 macOS
; Generates complete standalone executables

; ===== Mach-O Constants =====

; Magic numbers
(define MH-MAGIC-64 4277009103)        ; 0xFEEDFACF - 64-bit mach-o

; CPU types
(define CPU-TYPE-ARM64 16777228)       ; 0x0100000C
(define CPU-SUBTYPE-ARM64-ALL 0)

; File types
(define MH-EXECUTE 2)                  ; Executable file

; Flags
(define MH-NOUNDEFS 1)
(define MH-DYLDLINK 4)
(define MH-PIE 2097152)                ; 0x200000

; Load command types
(define LC-SEGMENT-64 25)              ; 0x19
(define LC-SYMTAB 2)
(define LC-DYSYMTAB 11)                ; 0x0B
(define LC-LOAD-DYLINKER 14)           ; 0x0E
(define LC-UUID 27)                    ; 0x1B
(define LC-BUILD-VERSION 50)           ; 0x32
(define LC-MAIN 2147483688)            ; 0x80000028 (LC_MAIN | LC_REQ_DYLD)
(define LC-LOAD-DYLIB 12)              ; 0x0C

; Segment/section constants
(define VM-PROT-READ 1)
(define VM-PROT-WRITE 2)
(define VM-PROT-EXECUTE 4)

(define S-ATTR-PURE-INSTRUCTIONS 2147483648)  ; 0x80000000
(define S-ATTR-SOME-INSTRUCTIONS 1024)        ; 0x400

; Platform
(define PLATFORM-MACOS 1)

; ===== Mach-O Writer =====

(define (make-macho)
  (let ((m (make-vector 4)))
    (vector-set! m 0 (make-vector 1048576))  ; output buffer (1MB)
    (vector-set! m 1 0)                       ; position
    (vector-set! m 2 0)                       ; code size
    (vector-set! m 3 0)                       ; entry offset
    m))

(define (macho-buf m) (vector-ref m 0))
(define (macho-pos m) (vector-ref m 1))
(define (macho-code-size m) (vector-ref m 2))
(define (macho-entry m) (vector-ref m 3))
(define (macho-set-pos! m p) (vector-set! m 1 p))
(define (macho-set-code-size! m s) (vector-set! m 2 s))
(define (macho-set-entry! m e) (vector-set! m 3 e))

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

; Emit string padded to length
(define (emit-string-m m str len)
  (emit-string-h m str 0 (string-length str) len))

(define (emit-string-h m str idx slen padlen)
  (if (>= idx padlen) 0
      (begin
        (if (< idx slen)
            (emit-byte-m m (string-ref str idx))
            (emit-byte-m m 0))
        (emit-string-h m str (+ idx 1) slen padlen))))

; Pad to alignment
(define (emit-padding-m m align)
  (let ((pos (macho-pos m)))
    (let ((rem (mod pos align)))
      (if (= rem 0) 0
          (emit-zeros m (- align rem))))))

(define (emit-zeros m count)
  (if (<= count 0) 0
      (begin (emit-byte-m m 0) (emit-zeros m (- count 1)))))

; ===== Mach-O Header Generation =====

; Generate minimal Mach-O header for ARM64 executable
(define (generate-macho-header m code-size)
  (let ((page-size 16384)             ; ARM64 macOS uses 16KB pages
        (header-size 4096)            ; We'll use 4KB for headers
        (text-vmaddr 4294983680)      ; 0x100004000 (standard load address + header)
        (base-vmaddr 4294967296))     ; 0x100000000

    ; Calculate sizes
    (let ((text-fileoff header-size)
          (text-size code-size)
          (segment-size (+ header-size code-size))
          (ncmds 4)
          (sizeofcmds 0))

      ; Mach-O header (32 bytes)
      (emit32-m m MH-MAGIC-64)                          ; magic
      (emit32-m m CPU-TYPE-ARM64)                       ; cputype
      (emit32-m m CPU-SUBTYPE-ARM64-ALL)                ; cpusubtype
      (emit32-m m MH-EXECUTE)                           ; filetype
      (emit32-m m ncmds)                                ; ncmds
      (emit32-m m 312)                                  ; sizeofcmds (calculated)
      (emit32-m m (bitor MH-NOUNDEFS (bitor MH-DYLDLINK MH-PIE))) ; flags
      (emit32-m m 0)                                    ; reserved

      ; LC_SEGMENT_64 for __PAGEZERO (72 bytes)
      (emit32-m m LC-SEGMENT-64)                        ; cmd
      (emit32-m m 72)                                   ; cmdsize
      (emit-string-m m "__PAGEZERO" 16)                 ; segname
      (emit64-m m 0)                                    ; vmaddr
      (emit64-m m base-vmaddr)                          ; vmsize (4GB null page)
      (emit64-m m 0)                                    ; fileoff
      (emit64-m m 0)                                    ; filesize
      (emit32-m m 0)                                    ; maxprot
      (emit32-m m 0)                                    ; initprot
      (emit32-m m 0)                                    ; nsects
      (emit32-m m 0)                                    ; flags

      ; LC_SEGMENT_64 for __TEXT (152 bytes: 72 header + 80 section)
      (emit32-m m LC-SEGMENT-64)                        ; cmd
      (emit32-m m 152)                                  ; cmdsize
      (emit-string-m m "__TEXT" 16)                     ; segname
      (emit64-m m base-vmaddr)                          ; vmaddr
      (emit64-m m (+ header-size code-size))            ; vmsize
      (emit64-m m 0)                                    ; fileoff
      (emit64-m m (+ header-size code-size))            ; filesize
      (emit32-m m (bitor VM-PROT-READ VM-PROT-EXECUTE)) ; maxprot
      (emit32-m m (bitor VM-PROT-READ VM-PROT-EXECUTE)) ; initprot
      (emit32-m m 1)                                    ; nsects
      (emit32-m m 0)                                    ; flags

      ; __text section header (80 bytes)
      (emit-string-m m "__text" 16)                     ; sectname
      (emit-string-m m "__TEXT" 16)                     ; segname
      (emit64-m m text-vmaddr)                          ; addr
      (emit64-m m code-size)                            ; size
      (emit32-m m header-size)                          ; offset
      (emit32-m m 2)                                    ; align (2^2 = 4)
      (emit32-m m 0)                                    ; reloff
      (emit32-m m 0)                                    ; nreloc
      (emit32-m m (bitor S-ATTR-PURE-INSTRUCTIONS S-ATTR-SOME-INSTRUCTIONS)) ; flags
      (emit32-m m 0)                                    ; reserved1
      (emit32-m m 0)                                    ; reserved2
      (emit32-m m 0)                                    ; reserved3

      ; LC_BUILD_VERSION (24 bytes)
      (emit32-m m LC-BUILD-VERSION)                     ; cmd
      (emit32-m m 24)                                   ; cmdsize
      (emit32-m m PLATFORM-MACOS)                       ; platform
      (emit32-m m 851968)                               ; minos (13.0 = 0xD0000)
      (emit32-m m 851968)                               ; sdk (13.0)
      (emit32-m m 0)                                    ; ntools

      ; LC_MAIN (24 bytes)
      (emit32-m m LC-MAIN)                              ; cmd
      (emit32-m m 24)                                   ; cmdsize
      (emit64-m m header-size)                          ; entryoff (offset to code)
      (emit64-m m 0)                                    ; stacksize (0 = default)

      ; Pad header to 4KB
      (emit-padding-m m header-size))))

; ===== Complete Binary Generation =====

(define (generate-binary code-bytes code-size)
  (let ((m (make-macho)))
    ; Generate Mach-O header
    (generate-macho-header m code-size)

    ; Copy code bytes
    (copy-code m code-bytes code-size)

    ; Return the macho structure
    m))

(define (copy-code m code-buf size)
  (copy-code-h m code-buf 0 size))

(define (copy-code-h m code-buf idx size)
  (if (>= idx size) 0
      (begin
        (emit-byte-m m (vector-ref code-buf idx))
        (copy-code-h m code-buf (+ idx 1) size))))

; Output binary to stdout
(define (output-binary m)
  (let ((buf (macho-buf m))
        (len (macho-pos m)))
    (output-binary-h buf 0 len)))

(define (output-binary-h buf idx len)
  (if (>= idx len) 0
      (begin
        (print (vector-ref buf idx))
        (output-binary-h buf (+ idx 1) len))))

; Write binary to file (for when we have file I/O)
(define (write-binary-file m filename)
  ; For now, just output the size
  (print (macho-pos m)))
