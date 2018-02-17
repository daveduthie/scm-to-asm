(load "util.scm")

(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.6-req.scm")
(load "tests-1.7-req.scm")
(load "tests-1.8-req.scm")

;;;; Helpers ----------------------------------------------------------------

(define fixnum-shift        2)
(define fixnum-tag       #b00)
(define fixnum-mask      #b11)
(define char-shift          8)
(define char-tag   #b00001111)
(define char-mask  #b11111111)
(define bool-shift          7)
(define bool-t     #b10011111)
(define bool-f     #b00011111)
(define bool-tag    #b0011111)
(define bool-mask   #b1111111)
(define empty-list #b00101111)
(define nil        empty-list)
(define (emit . args)
  (display (apply format args))
  (newline))

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
   [(char? x)
    (bitwise-ior (ash (char->integer x) char-shift) char-tag)]
   [else
    (error 'emit-program
           "Cannot find representation for this element: " x)]))

(define (emit-immediate tail? x)
  (emit "  mov rax, ~s" (immediate-rep x))
  (if tail? (emit "  ret")))

;;;; Primitive calls ----------------------------------------------------------------

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                (lambda (si env arg* ...) b b* ...)))]
    [(_ (prim-name si env . args) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*variadic?* #t)
       (putprop 'prim-name '*emitter*
                (lambda (si env . args) b b* ...)))] ))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter
             "Raargh.. cannot find an emitter for:" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (unless (or (getprop prim '*variadic?*)
              (eq? (length args) (getprop prim '*arg-count*)))
    (errorf
     'check-primcall-args
     "Wrong number of args (~s) for expr: ~s.~%~s takes ~s args.~%"
     args prim prim (getprop prim '*arg-count*))))

(define (emit-primcall si env tail? expr)
  (let [(prim (car expr)) (args (cdr expr))]
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)
    (if tail? (emit "  ret"))))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env #f arg)
  (emit "  add rax, ~s" (immediate-rep 1)))

(define-primitive (fxsub1 si env arg)
  (emit-expr si env #f arg)
  (emit "  sub rax, ~s" (immediate-rep 1)))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env #f arg)
  (emit "  shl rax, ~s" (- char-shift fixnum-shift))
  (emit "  or rax, ~s" char-tag))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env #f arg)
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
    [(_ (prim-name si env arg) b* ...)
     (define-primitive (prim-name si env arg)
       (emit-expr si env #f arg)
       b* ...
       (emit "  setz al")
       (emit "  movzx rax, al")
       (emit "  shl rax, ~s" bool-shift)
       (emit "  or rax, ~s ; end predicate" bool-tag))]))

(define-predicate (fixnum? si env arg)
  (emit "  and rax, ~s" fixnum-mask))

(define-predicate (fxzero? si env arg)
  (emit "  cmp rax, ~s" fixnum-tag))

(define-predicate (null? si env arg)
  (emit "  cmp rax, ~s" empty-list))

(define-predicate (boolean? si env arg)
  (emit "  and rax, ~s" bool-mask)
  (emit "  cmp rax, ~s" bool-tag))

(define-predicate (char? si env arg)
  (emit "  and rax, ~s" char-mask)
  (emit "  cmp rax, ~s" char-tag))

(define-predicate (not si env arg)
  (emit "  cmp rax, ~s" bool-f))

(define-primitive (fxlognot si env arg)
  (emit-expr si env #f arg)
  (emit "  or rax, ~s" fixnum-mask)
  (emit "  not rax"))

;;;; Labels --------------------------------------------------------------------

(define (mk-labeller)
  (let ([count 0])
    (case-lambda
      [() (unique-label "")]
      [(x) (let ([L (format "L_~s_~a" count x)])
             (set! count (add1 count))
             L)])))

(define unique-label (mk-labeller))

(define (reset-unique-label)
  (set! unique-label (mk-labeller)))

;;;; If form ----------------------------------------------------------------

(define (if? expr)
  (and (pair? expr) (equal? (car expr) 'if)))

(define (emit-if si env tail? expr)
  (let ([test (cadr expr)]
        [consequent (caddr expr)]
        [alternate (cadddr expr)]
        [alt-label (unique-label "alt")]
        [end-label (unique-label "end")])
    (emit-expr si env #f test)
    (emit "  cmp rax, ~s ; cmp #f" bool-f)
    (emit "  je ~a" alt-label)
    (emit "  cmp rax, ~s ; cmp ()" nil)
    (emit "  je ~a" alt-label)
    (emit-expr si env tail? consequent)
    (emit "  jmp ~a" end-label)
    (emit "~a: ; alt" alt-label)
    (emit-expr si env tail? alternate)
    (emit "~a: ; end" end-label)))

;;;; And form ----------------------------------------------------------------

(define (and? expr)
  (and (pair? expr) (equal? (car expr) 'and)))

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
  (and (pair? expr) (equal? (car expr) 'or)))

;;;; Higher-arity primitives ----------------------------------------------------------------

(define (build-on-stack si env args)
  (let loop ([offset (+ si wordsize)] [args args])
    (unless (null? args)
      (emit-expr offset env #f (car args))
      (unless (null? (cdr args))
        (emit "  mov [rsp - ~a], rax" offset))
      (loop (+ offset wordsize) (cdr args)))))

(define (reduce-stack si env n rf fail)
  (let ([top (+ si (* n wordsize))])
    (rf top env fail)
    (let loop ([top (- top wordsize)])
      (unless (eq? top si)
        (rf top env fail)
        (loop (- top wordsize)))
      (emit "  ; end of reduce-stack"))))

(define-syntax define-primitive-reduction
  (syntax-rules ()
    [(_ (prim-name si env . args) rf init)
     (define-primitive (prim-name si env . args)
       (let ([rev (reverse args)]
             [len (length args)])
         (cond
          [(eq? len 0) (emit "  mov rax, ~s" (immediate-rep init))]
          [(eq? len 1) (emit-expr si env #t (car args))]
          [else        (begin
                         (emit "  ; emitting build on stack (si: ~s)" si)
                         (build-on-stack si env rev)
                         (reduce-stack si env (sub1 len) rf nil))])))]))

(define-primitive-reduction (fx+ si env . args)
  (lambda (si env fail) (emit "  add rax, [rsp - ~a]" si)) 0)

(define-primitive-reduction (fx- si env . args)
  (lambda (si env fail) (emit "  sub rax, [rsp - ~a]" si)) 0)

(define-primitive-reduction (fx* si env . args)
  (lambda (si env fail)
    (emit "  shr rax, 2")
    (emit "  imul rax, [rsp - ~a]" si)) 1)

(define-primitive-reduction (fxlogor si env . args)
  (lambda (si env fail) (emit "  or rax, [rsp - ~a]" si)) 0)

(define-primitive-reduction (fxlogand si env . args)
  (lambda (si env fail) (emit "  and rax, [rsp - ~a]" si)) 0)

(define-syntax define-primitive-variadic-predicate
  (syntax-rules ()
    [(_ (prim-name si env . args) rf)
     (define-primitive (prim-name si env . args)
       (let ([fail (unique-label "fail")]
             [end  (unique-label "end")]
             [rev  (reverse args)]
             [len  (length args)])
         (cond
          [(eq? len 0) (error 'primitive-variadic
                              "Cannot compare nothing")]
          [(eq? len 1) (emit-expr si env bool-t)]
          [else        (begin
                         (build-on-stack si env rev)
                         (reduce-stack si env (sub1 len) rf fail)
                         ;; if control reaches this point, args are equal
                         (emit "  mov rax, ~s" bool-t)
                         (emit " jmp ~a" end)
                         (emit "~a: ; not= label" fail)
                         (emit "  mov rax, ~s" bool-f)
                         (emit "~a: ; end label" end))])))]))

(define-primitive-variadic-predicate (fx= si env . args)
  (lambda (si env fail)
    (emit "  cmp rax, [rsp - ~a]" si)
    (emit "  jne ~a" fail)))

(define-primitive-variadic-predicate (fx< si env . args)
  (lambda (si env fail)
    (emit "  cmp rax, [rsp - ~a]" si)
    (emit "  jnl ~a" fail)))

(define-primitive-variadic-predicate (fx<= si env . args)
  (lambda (si env fail)
    (emit "  cmp rax, [rsp - ~a]" si)
    (emit "  jnle ~a" fail)))

(define-primitive-variadic-predicate (fx> si env . args)
  (lambda (si env fail)
    (emit "  cmp rax, [rsp - ~a]" si)
    (emit "  jng ~a" fail)))

(define-primitive-variadic-predicate (fx>= si env . args)
  (lambda (si env fail)
    (emit "  cmp rax, [rsp - ~a]" si)
    (emit "  jnge ~a" fail)))


;;;; Let bindings ----------------------------------------------------------------
;;; Can't I desugar these into lambdas?
;;; Oh, I don't have lambdas yet :P

(define (let? expr)
  (and (pair? expr) (equal? (car expr) 'let)))

(define (emit-stack-save si)
  (emit "  mov [rsp - ~s], rax" si))

(define (extend-env sym si env)
  (cons (cons sym si) env))

(define (emit-let si env tail? expr)
  (define (process-let bindings si new-env)
    (if (empty? bindings)
        (let ([tail-expr (car (reverse (cddr expr)))])
          (for-each (lambda (exp) (if (equal? exp tail-expr) ; FIXME: I'm ugly
                                      (emit-expr si new-env tail? exp)
                                      (emit-expr si new-env #f exp)))
                    (cddr expr)))
        (let ([b (car bindings)]
              [nsi (+ si wordsize)])
          (emit-expr si new-env #f (cadr b))
          (emit-stack-save nsi)
          (process-let (cdr bindings)
                       nsi
                       (extend-env (car b) nsi new-env)))))
  (process-let (partition-all 2 (cadr expr)) si env))

(define (variable? expr)
  (symbol? expr))

(define (lookup sym env)
  (cdr (assoc sym env)))

(define (emit-stack-load si)
  (emit "  mov rax, [rsp - ~s]" si))

(define (emit-variable-ref env tail? sym)
  (cond [(lookup sym env) => emit-stack-load]
        [else (errorf 'emit-variable-ref "Symbol: ~s is unbound" sym)])
  (if tail? (emit "  ret")))

;;;; Letrec (lambdas) ----------------------------------------------------------------

(define (letrec? expr)
  (and (pair? expr) (equal? (car expr) 'letrec)))

(define (emit-letrec si env tail? expr)
  (let*  ([bindings  (partition-all 2 (cadr expr))]
          [lvars     (map car bindings)]
          [lambdas   (map cadr bindings)]
          [labels    (map (lambda (x) (unique-label "lambda")) lvars)]
          [env       (append (map cons lvars labels) env)]
          [rev-args  (reverse (cddr expr))]
          [tail-expr (car rev-args)]
          [args      (reverse (cdr rev-args))])

    ;; compile lambdas
    (for-each (emit-lambda env) lambdas labels)

    ;; body of letrec
    ;; (printf "letrec body: ( ~s ) : ~s" args tail-expr)
    (for-each (lambda (exp) (emit-expr si env #f exp)) args)
    (emit-expr si env tail? tail-expr)))

(define user-lambdas '())

(define (emit-lambda env)
  (lambda (expr label)
    (set! user-lambdas
     (cons
      (lambda ()
        (emit-function-header label)
        (let* ([fmls      (cadr expr)]
               [rev-exprs (reverse (cddr expr))]
               [tail-expr (car rev-exprs)]
               [exprs     (reverse (cdr rev-exprs))])
          (let f ([fmls fmls] [si wordsize] [env env])
            (cond [(empty? fmls)
                   (begin (for-each (lambda (exp)
                                      (emit-expr si env #f exp)) exprs)
                          (emit-expr si env #t tail-expr))]
                  [else (f (cdr fmls)
                           (+ si wordsize)
                           (extend-env (car fmls) si env))]))))
      user-lambdas))))

;;;; Function application

(define (app? expr)
  ;; For this work, app? must be evaluated after other syntax-matchers
  (and (pair? expr) (symbol? (car expr))))

(define (shift-stack-frame-down si n)
  (let ([shift (+ si wordsize)])
    (emit "  ; --------------------------------- shifting stack frame of ~s args down by ~s <<*>>" n shift)
    (let loop ([x n]
               [offset wordsize])
      (unless (zero? x)
        (emit "  mov rax, [rsp - ~s]" (+ offset shift))
        (emit "  mov [rsp - ~s], rax" offset)
        (loop (sub1 x) (+ offset wordsize))))))

(define (emit-app si env tail? expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env #f (car args))
      (emit "  mov [rsp - ~s], rax ; passing arg to ~s" si (car expr))
      (emit-arguments (+ si wordsize) (cdr args))))
  (emit "  ; si is ~s" si)
  (let* ([args (cdr expr)]
         [base (+ si (* 2 wordsize))])
    (emit "  ; --------------------------------- emitting args before app call <<*>>")
    (emit-arguments base args)
    (if tail?
        (begin (shift-stack-frame-down si (length args))
               (emit "  jmp ~a ; app call" (lookup (car expr) env)))
        (begin (emit "  sub rsp, ~s" si)
               (emit "  call ~a ; app call" (lookup (car expr) env))
               (emit "  add rsp, ~s" si)))))

;;;; Context-switch ----------------------------------------------------------------

(define registers
  '((rax scratch)
    (rbx preserve)
    (rcx scratch)
    (rdx scratch)
    (rsi preserve)
    (rdi preserve)
    (rbp preserve)
    (rsp preserve)))

(define (reg-name reg) (car reg))

(define (reg-preserve? reg) (eq? 'preserve (cadr reg)))

(define (preserve-registers cmd)
  (let loop ([regs registers] [count 0])
    (unless (null? regs)
      (let ([reg (car regs)])
        (when (reg-preserve? reg)
          (cmd (reg-name reg)
               (* count wordsize))))
      (loop (cdr regs) (add1 count)))))

(define (backup-registers)
  (preserve-registers (lambda (name num)
                        (emit "  mov [rdi + ~a], ~s" num name))))

(define (restore-registers)
  (preserve-registers (lambda (name num)
                        (emit "  mov ~s, [rdi + ~a]" name num))))

;;;; Compiler ----------------------------------------------------------------

(define (emit-expr si env tail? expr)
  ;; TODO: rewrite with pattern-matching
  (cond [(immediate? expr) (emit-immediate tail? expr)]
        [(if? expr)        (emit-if si env tail? expr)]
        [(and? expr)       (emit-if si env tail? (transform-and expr))]
        [(or? expr)        (emit-if si env tail? (transform-or expr))]
        [(primcall? expr)  (emit-primcall si env tail? expr)]
        [(variable? expr)  (emit-variable-ref env tail? expr)]
        [(let? expr)       (emit-let si env tail? expr)]
        [(letrec? expr)    (emit-letrec si env tail? expr)]
        [(app? expr)       (if (equal? 'app (car expr))
                               (error 'emit-expr "encountered 'app'")
                               (emit-app si env tail? expr))]
        [else
         (error 'emit-expr
                (format "Cannot encode this peanut: ~s" expr))]))

(define (emit-function-header name)
  (emit "global ~a" name)
  (emit "~a:" name))

(define (emit-program expr)
  (set! user-lambdas '()) ; clear from previous invocations
  (emit-function-header "L_scheme_entry")
  (emit-expr 0 '() #t expr)
  (emit-function-header "scheme_entry")
  (emit "  ; expect rdi <- &ctx,")
  (emit "  ; rsi <- stack_base, rdx <- heap_base")
  (backup-registers)
  (emit "  mov rsp, rsi ; move stack base into rsp")
  (emit "  mov rbp, rdx ; move heap base into rbp")
  (emit "  mov rsp, rsi")
  (emit "  call L_scheme_entry")
  (restore-registers)
  (emit "  ret")
  (for-each (lambda (emitter) (emitter))
            user-lambdas))
