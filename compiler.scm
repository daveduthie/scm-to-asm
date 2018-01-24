(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")

;;;; Helpers ----------------------------------------------------------------

;; Fixnum shape => ------------------------------11
(define fixnum-shift 2)
(define fixnum-tag #b00)
(define fixnum-mask #b11)

;; Char shape   => ------------------------00001111
(define char-shift 8)
(define char-mask #b11111111)
(define char-tag #b00001111)
;; Bool shape   =>                         _0011111
(define bool-shift 7)
(define bool-t #b10011111)
(define bool-f #b00011111)
(define bool-tag #b0011111)
(define bool-mask #b1111111)
;; Empty list   =>                         00101111
(define empty-list #b00101111)

;;;; Immediate values ----------------------------------------------------------------

(define wordsize 8)
(define fxbits (- (* wordsize 8) fixnum-shift))
(define fxlower (- (expt 2 (- fxbits 1))))
(define fxupper (sub1 (expt 2 (- fxbits 1))))
(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (equal? x '())))

(define (immediate-rep x)
  (cond
   [(fixnum? x)    (ash x fixnum-shift)]
   [(boolean? x)   (if x bool-t bool-f)]
   [(equal? x '()) empty-list]
   [(char? x)      (bitwise-ior (ash (char->integer x) char-shift) char-tag)]
   [else           (error 'emit-program "Cannot find representation for this element: " x)]))

(define (emit-immediate x)
  (emit "  mov rax, ~s" (immediate-rep x)))

;;;; Primitive calls ----------------------------------------------------------------

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter "Raargh.. cannot find an emitter for:" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (emit-primcall expr)
  (let [(prim (car expr)) (args (cdr expr))]
    ;; (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define-primitive (fxadd1 arg)
  (emit-expr arg)
  (emit "  add rax, ~s" (immediate-rep 1)))

(define-primitive (fxsub1 arg)
  (emit-expr arg)
  (emit "  sub rax, ~s" (immediate-rep 1)))

(define-primitive (fixnum->char arg)
  (emit-expr arg)
  (emit "  shl rax, ~s" (- char-shift fixnum-shift))
  (emit "  or rax, ~s" char-tag))

(define-primitive (char->fixnum arg)
  (emit-expr arg)
  (emit "  shr rax, ~s" char-shift)
  (emit "  shl rax, ~s" fixnum-shift))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "  and rax, ~s" fixnum-mask)
  (emit "  setz al")
  (emit "  movzx rax, al")
  (emit "  shl rax, ~s" bool-shift)
  (emit "  or rax, ~s" bool-tag))

(define-primitive (fxzero? arg)
  (emit-expr arg)
  (emit "  cmp rax, ~s" fixnum-tag)
  (emit "  sete al")
  (emit "  movzx rax, al")
  (emit "  shl rax, ~s" bool-shift)
  (emit "  or rax, ~s" bool-tag))

(define-primitive (null? arg)
  (emit-expr arg)
  (emit "  cmp rax, ~s" empty-list)
  (emit "  sete al")
  (emit "  movzx rax, al")
  (emit "  shl rax, ~s" bool-shift)
  (emit "  or rax, ~s" bool-tag))

(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "  and rax, ~s" bool-mask)
  (emit "  cmp rax, ~s" bool-tag)
  (emit "  sete al")
  (emit "  movzx rax, al")
  (emit "  shl rax, ~s" bool-shift)
  (emit "  or rax, ~s" bool-tag))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "  and rax, ~s" char-mask)
  (emit "  cmp rax, ~s" char-tag)
  (emit "  sete al")
  (emit "  movzx rax, al")
  (emit "  shl rax, ~s" bool-shift)
  (emit "  or rax, ~s" bool-tag))


(define-primitive (not arg)
  (emit-expr arg)
  (emit "  cmp rax, ~s" bool-f)
  (emit "  sete al")
  (emit "  movzx rax, al")
  (emit "  shl rax, ~s" bool-shift)
  (emit "  or rax, ~s" bool-tag))

(define-primitive (fxlognot arg)
  (emit-expr arg)
  (emit "  or rax, ~s" fixnum-mask)
  (emit "  not rax"))

(define (emit-expr expr)
  (cond [(immediate? expr) (emit-immediate expr)]
        [(primcall? expr) (emit-primcall expr)]
        [else (error 'emit-expr "Cannot encode this peanut:" expr)]))

(define (emit-header)
  (emit "global scheme_entry")
  (emit "section .text")
  (emit "scheme_entry:"))

(define (emit-program expr)
  (emit-header)
  (emit-expr expr)
  (emit "  ret"))
