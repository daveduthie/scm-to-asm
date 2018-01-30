(load "util.scm")

;; (load "tests-1.1-req.scm")
;; (load "tests-1.2-req.scm")
;; (load "tests-1.3-req.scm")
;; (load "tests-1.4-req.scm")
(load "tests-1.5-req.scm")


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
(define nil empty-list)

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
    [(_ (prim-name si arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si arg* ...) b b* ...)))]
    [(_ (prim-name si . args) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*variadic?* #t)
       (putprop 'prim-name '*emitter*
                (lambda (si . args) b b* ...)))] ))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter "Raargh.. cannot find an emitter for:" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (unless (or (getprop prim '*variadic?*)
              (eq? (length args) (getprop prim '*arg-count*)))
    (errorf 'check-primcall-args
            "Wrong number of args (~s) for expr: ~s.~%~s takes ~s args.~%"
            args prim prim (getprop prim '*arg-count*))))

(define (emit-primcall si expr)
  (let [(prim (car expr)) (args (cdr expr))]
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))

(define-primitive (fxadd1 si arg)
  (emit-expr si arg)
  (emit "  add rax, ~s" (immediate-rep 1)))

(define-primitive (fxsub1 si arg)
  (emit-expr si arg)
  (emit "  sub rax, ~s" (immediate-rep 1)))

(define-primitive (fixnum->char si arg)
  (emit-expr si arg)
  (emit "  shl rax, ~s" (- char-shift fixnum-shift))
  (emit "  or rax, ~s" char-tag))

(define-primitive (char->fixnum si arg)
  (emit-expr si arg)
  (emit "  shr rax, ~s" char-shift)
  (emit "  shl rax, ~s" fixnum-shift))

;;; Many primitives are unary predicates which return true or false.
;;; This macro factors out some of the boilerplate for these predicates.
;;; To implement a new predicate, it is enough to emit assembly which
;;; ends with a comparison operator. The lsb in rax will be be taken to
;;; indicate the truth or falsity of the expression.
;;; See below for examples
(define-syntax define-predicate
  (syntax-rules ()
    [(_ (prim-name si arg) b* ...)
     (define-primitive (prim-name si arg) ; 0 is unused stack pointer
       (emit-expr si arg)
       b* ...
       (emit "  setz al")
       (emit "  movzx rax, al")
       (emit "  shl rax, ~s" bool-shift)
       (emit "  or rax, ~s" bool-tag))]))

(define-predicate (fixnum? si arg)
  (emit "  and rax, ~s" fixnum-mask))

(define-predicate (fxzero? si arg)
  (emit "  cmp rax, ~s" fixnum-tag))

(define-predicate (null? si arg)
  (emit "  cmp rax, ~s" empty-list))

(define-predicate (boolean? si arg)
  (emit "  and rax, ~s" bool-mask)
  (emit "  cmp rax, ~s" bool-tag))

(define-predicate (char? si arg)
  (emit "  and rax, ~s" char-mask)
  (emit "  cmp rax, ~s" char-tag))

(define-predicate (not si arg)
  (emit "  cmp rax, ~s" bool-f))

(define-primitive (fxlognot si arg)
  (emit-expr si arg)
  (emit "  or rax, ~s" fixnum-mask)
  (emit "  not rax"))


;;;; Higher-arity primitives ----------------------------------------------------------------

(define (build-on-stack si args)
  ;; (emit ";;; BUILDING STACK FROM ~s WITH ~s" si args)
  (let loop ([si si] [args args])
    (unless (null? args)
      (emit-expr si (car args))
      (emit "  mov [rsp - ~a], rax" (+ si wordsize))
      (loop (+ si wordsize) (cdr args)))))

(define (reduce-stack si n rf)
  (let ([top (+ si (* n wordsize))])
    ;; (emit ";;; REDUCING FROM ~s TO ~s" top si)
    (emit "  mov rax, [rsp - ~a]" top)
    (let reducer ([top (- top wordsize)])
      (unless (eq? top si)
        (rf top)
        (reducer (- top wordsize))))))

(define-syntax define-primitive-reduction
  (syntax-rules ()
    [(_ (prim-name si . args) rf init body* ...)
     (define-primitive (prim-name si . args)
       (let ([rev   (reverse args)]
             [len (length args)])
         (cond
          [(eq? len 0) (emit "  mov rax, ~s" (immediate-rep init))]
          [(eq? len 1) (emit-expr si (car args))]
          [else        (begin ;; (emit-expr si (car rev))
                              (build-on-stack si rev)
                              (reduce-stack si len rf))])))]))

(define-primitive-reduction (fx+ si . args)
  (lambda (si) (emit "  add rax, [rsp - ~a]" si)) 0)

(define-primitive-reduction (fx- si . args)
  (lambda (si) (emit "  sub rax, [rsp - ~a]" si)) 0)

;;; Multiplication is difficult because of the bit shift.
;;; How about: shift each arg right by two before multiplying?

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

(define (emit-if si expr)
  ;; (printf "CALLED IF ~s ~%" expr)
  (let ([test (cadr expr)]
        [consequent (caddr expr)]
        [alternate (cadddr expr)]
        [alt-label (unique-label)]
        [end-label (unique-label)])
    ;; (emit ";;;; ~s:~s:~s" test consequent alternate)
    (emit-expr si test)
    (emit "  cmp rax, ~s ; cmp #f" bool-f)
    (emit "  je ~a" alt-label)
    (emit "  cmp rax, ~s ; cmp ()" nil)
    (emit "  je ~a" alt-label)
    (emit-expr si consequent)
    (emit "  jmp ~a" end-label)
    (emit "~a: ; alt" alt-label)
    (emit-expr si alternate)
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

;;;; Or form ----------------------------------------------------------------

(define (transform-or expr)
  (let alternate ([i (cdr expr)])
    (if (null? i)
        #f
        `(if ,(car i) #t ,(alternate (cdr i))))))

(define (or? expr)
  (equal? (car expr) 'or))

;;;; Compiler ----------------------------------------------------------------

(define (emit-expr si expr)
  ;; (emit ";;; EMIT ~s AT ~s" expr si)
  (cond [(immediate? expr) (emit-immediate expr)]
        [(if? expr)        (emit-if si expr)]
        [(and? expr)       (emit-if si (transform-and expr))]
        [(or? expr)        (emit-if si (transform-or expr))]
        [(primcall? expr)  (emit-primcall si expr)]
        [else              (error 'emit-expr (format "Cannot encode this peanut: ~s" expr))]))

(define (emit-function-header name)
  (emit "global ~a" name)
  (emit "~a:" name))

(define (emit-program expr)
  ;; (emit "section .text")
  (emit-function-header "L_scheme_entry")
  (emit-expr 0 expr)
  (emit "  ret")
  (emit-function-header "scheme_entry")
  (emit "  mov rcx, rsp")
  (emit "  mov [rsp + ~a], rsp" wordsize)
  (emit "  call L_scheme_entry")
  (emit "  mov rsp, rcx")
  (emit "  ret"))
