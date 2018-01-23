(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")

;; Fixnum shape => ------------------------------11
(define fixnum-shift 2)
;; Char shape   => ------------------------00001111
(define char-shift 8)
(define char-tag #b00001111)
;; Bool shape   =>                         _0011111
(define bool-t #b10011111)
(define bool-f #b00011111)
;; Empty list   =>                         00101111
(define empty-list #b00101111)

(define (emit-program x)
  (define (immediate-rep x)
    (cond
     [(integer? x)   (ash x fixnum-shift)]
     [(boolean? x)   (if x bool-t bool-f)]
     [(equal? x '()) empty-list]
     [(char? x)      (bitwise-ior (ash (char->integer x) char-shift) char-tag)]
     [else           (error 'emit-program "Cannot find representation for this element: " x)]))
  (emit "global scheme_entry")
  (emit "section .text")
  (emit "scheme_entry:")
  (emit "  mov rax, ~s" (immediate-rep x))
  (emit "  ret"))
