(load "util.scm")

(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.4-req.scm")


;;;; Helpers ----------------------------------------------------------------

(define (emit . args)
  (display (apply format args))
  (newline))

;; Fixnum shape => ------------------------------11
(define fixnum-shift 2)
(define fixnum-tag #b00)
(define fixnum-mask #b11)

;; Char shape   => ------------------------00001111
(define char-shift 8)
(define char-tag  #b00001111)
(define char-mask #b11111111)
;; Bool shape   =>                         _0011111
(define bool-shift 7)
(define bool-t #b10011111)
(define bool-f #b00011111)
(define bool-tag #b0011111)
(define bool-mask #b1111111)
;; Empty list   =>                         00101111
(define empty-list #b00101111)
(define nil #b00101111)

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
  (emit "  mov eax, ~s" (immediate-rep x)))

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
  (emit "  add eax, ~s" (immediate-rep 1)))

(define-primitive (fxsub1 arg)
  (emit-expr arg)
  (emit "  sub eax, ~s" (immediate-rep 1)))

(define-primitive (fixnum->char arg)
  (emit-expr arg)
  (emit "  shl eax, ~s" (- char-shift fixnum-shift))
  (emit "  or eax, ~s" char-tag))

(define-primitive (char->fixnum arg)
  (emit-expr arg)
  (emit "  shr eax, ~s" char-shift)
  (emit "  shl eax, ~s" fixnum-shift))

;;; Many primitives are unary predicates which return true or false.
;;; This macro factors out some of the boilerplate for these predicates.
;;; To implement a new predicate, it is enough to emit assembly which
;;; checks the value in eax/rax and sets the lsb in al: 1 -> #t or 0 -> #f
;;; See below for examples
(define-syntax define-predicate
  (syntax-rules ()
    [(_ (prim-name arg) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count* 1)
       (putprop 'prim-name '*emitter*
                (lambda (arg)
                  (emit-expr arg)
                  b b* ...
                  (emit "  movzx eax, al")
                  (emit "  shl eax, ~s" bool-shift)
                  (emit "  or eax, ~s" bool-tag))))]))

(define-predicate (fixnum? arg)
  (emit "  and eax, ~s" fixnum-mask)
  (emit "  setz al"))

(define-predicate (fxzero? arg)
  (emit "  cmp eax, ~s" fixnum-tag)
  (emit "  sete al"))

(define-predicate (null? arg)
  (emit "  cmp eax, ~s" empty-list)
  (emit "  sete al"))

(define-predicate (boolean? arg)
  (emit "  and eax, ~s" bool-mask)
  (emit "  cmp eax, ~s" bool-tag)
  (emit "  sete al"))

(define-predicate (char? arg)
  (emit "  and eax, ~s" char-mask)
  (emit "  cmp eax, ~s" char-tag)
  (emit "  setz al"))


(define-predicate (not arg)
  (emit "  cmp eax, ~s" bool-f)
  (emit "  sete al"))

(define-primitive (fxlognot arg)
  (emit-expr arg)
  (emit "  or eax, ~s" fixnum-mask)
  (emit "  not eax"))

;;;; If form ----------------------------------------------------------------

;;; Require programmatic access to unique labels
(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        L))))

(define (if? expr)
  (equal? (car expr) 'if))

(define (emit-if expr)
  ;; (printf "CALLED IF ~s ~%" expr)
  (let ([test (cadr expr)]
        [consequent (caddr expr)]
        [alternate (cadddr expr)]
        [alt-label (unique-label)]
        [end-label (unique-label)])
    (emit ";;;; ~s:~s:~s" test consequent alternate)
    (emit-expr test)
    (emit "  cmp eax, ~s ; cmp #f" bool-f)
    (emit "  je ~a" alt-label)
    (emit "  cmp eax, ~s ; cmp ()" nil)
    (emit "  je ~a" alt-label)
    (emit-expr consequent)
    (emit "  jmp ~a" end-label)
    (emit "~a: ; alt" alt-label)
    (emit-expr alternate)
    (emit "~a: ; end" end-label)))

;;;; And form ----------------------------------------------------------------

(define (and? expr)
  (equal? (car expr) 'and))

;; TODO: Study this macro
;; Transform an and expression into a nested if expression.
;; (and a b ...)
;; (if a (if b #t #f) #f)
(define (transform-and expr)
  (let conseq ([i (cdr expr)])
    (if (null? i)
        #t
        `(if ,(car i) ,(conseq (cdr i)) #f))))

;;;; Compiler ----------------------------------------------------------------

(define (emit-expr expr)
  (cond [(immediate? expr) (emit-immediate expr)]
        [(if? expr)        (emit-if expr)]
        [(and? expr)       (emit-if (transform-and expr))]
        [(primcall? expr)  (emit-primcall expr)]
        [else              (error 'emit-expr (format "Cannot encode this peanut: ~s" expr))]))

(define (emit-header)
  (emit "global scheme_entry")
  (emit "section .text")
  (emit "scheme_entry:"))

(define (emit-program expr)
  (emit-header)
  (emit-expr expr)
  (emit "  ret"))
