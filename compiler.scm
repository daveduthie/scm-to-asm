(load "util.scm")

(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.6-req.scm")
(load "tests-1.7-req.scm")

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
      (error 'primitive-emitter "Raargh.. cannot find an emitter for:" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (unless (or (getprop prim '*variadic?*)
              (eq? (length args) (getprop prim '*arg-count*)))
    (errorf 'check-primcall-args
            "Wrong number of args (~s) for expr: ~s.~%~s takes ~s args.~%"
            args prim prim (getprop prim '*arg-count*))))

(define (emit-primcall si env expr)
  (let [(prim (car expr)) (args (cdr expr))]
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "  add rax, ~s" (immediate-rep 1)))

(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "  sub rax, ~s" (immediate-rep 1)))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "  shl rax, ~s" (- char-shift fixnum-shift))
  (emit "  or rax, ~s" char-tag))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
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
     (define-primitive (prim-name si env arg) ; 0 is unused stack pointer
       (emit-expr si env arg)
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
  (emit-expr si env arg)
  (emit "  or rax, ~s" fixnum-mask)
  (emit "  not rax"))


;;;; If form ----------------------------------------------------------------

;;; Require programmatic access to unique labels
(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        L))))

(define (if? expr)
  (and (pair? expr) (equal? (car expr) 'if)))

(define (emit-if si env expr)
  (let ([test (cadr expr)]
        [consequent (caddr expr)]
        [alternate (cadddr expr)]
        [alt-label (unique-label)]
        [end-label (unique-label)])
    (emit-expr si env test)
    (emit "  cmp rax, ~s ; cmp #f" bool-f)
    (emit "  je ~a" alt-label)
    (emit "  cmp rax, ~s ; cmp ()" nil)
    (emit "  je ~a" alt-label)
    (emit-expr si env consequent)
    (emit "  jmp ~a" end-label)
    (emit "~a: ; alt" alt-label)
    (emit-expr si env alternate)
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
  (let loop ([si si] [args args])
    (unless (null? args)
      (emit-expr si env (car args))
      (unless (null? (cdr args))
        (emit "  mov [rsp - ~a], rax" (+ si wordsize)))
      (loop (+ si wordsize) (cdr args)))))

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
          [(eq? len 1) (emit-expr si env (car args))]
          [else        (begin
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
       (let ([fail (unique-label)]
             [end  (unique-label)]
             [rev  (reverse args)]
             [len  (length args)])
         (cond
          [(eq? len 0) (error 'primitive-variadic "Cannot compare nothing")]
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

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (if (empty? bindings)
        (emit-expr si new-env (caddr expr))
        (let ([b (car bindings)]
              [nsi (+ si wordsize)])
          (emit-expr si new-env (cadr b))
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

(define (emit-variable-ref env sym)
  (cond [(lookup sym env) => emit-stack-load]
        [else (errorf 'emit-variable-ref "Symbol: ~s is unbound" sym)]))

;;;; Letrec (lambdas) ----------------------------------------------------------------

(define (letrec? expr)
  (and (pair? expr) (equal? (car expr) 'letrec)))

(define (emit-letrec si env expr)
  ;; (printf "~%Trying to compile letrec: ~s~%" expr)
  (let*  ([bindings (partition-all 2 (cadr expr))]
          [lvars   (map car bindings)]
          [lambdas (map cadr bindings)]
          [labels  (map (lambda (x) (unique-label)) lvars)]
          [env     (append (map cons lvars labels) env)])

    ;; (printf "~%lvars: ~s~%lambdas: ~s~%labels: ~s~%env: ~s~%" lvars lambdas labels env)

    ;; compile lambdas
    (for-each (emit-lambda env) lambdas labels)

    ;; body of letrec
    (for-each (lambda (exp) (emit-expr si env exp)) (cddr expr)) ; OK?
    ))

(define user-lambdas '())

(define (emit-lambda env)
  ;; (printf "Defining lambda! with env: ~s" env)
  (lambda (expr label)
    ;; (printf "Called lambda with expr, label: ~s ~s" expr label)
    ;; (emit "  ; lambda got called here wth expr: ~s label: ~s" expr label)
    (set! user-lambdas
     (cons
      (lambda ()
        (emit-function-header label)
        (let ([fmls (cadr expr)]
              [body (cddr expr)])
          ;; (printf "fmls: ~s body: ~s" fmls body)
          (let f ([fmls fmls] [si wordsize] [env env])
            (cond [(empty? fmls) (for-each (lambda (exp)
                                             ;; (printf "Emitting expr for ~s" exp)
                                             (emit-expr si env exp)
                                             (emit "  ret")) body)]
                  [else (f (cdr fmls)
                           (+ si wordsize)
                           (extend-env (car fmls) si env))]))))
      user-lambdas))))

;;;; Function application

(define (app? expr)
  ;; For this work, app? must be evaluated after other syntax-matchers
  (and (pair? expr) (symbol? (car expr))))

(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env (car args))
      (emit "  mov [rsp - ~s], rax" si)
      (emit-arguments (+ si wordsize) (cdr args))))
  (emit-arguments (+ si (* wordsize 2)) (cdr expr))
  (emit "  sub rsp, ~s" si)
  (emit "  call ~a ; app call" (lookup (car expr) env))
  (emit "  add rsp, ~s" si))

;;;; Compiler ----------------------------------------------------------------

(define (emit-expr si env expr)
  ;; TODO: rewrite with pattern-matching
  ;; (emit "  ; emit-expr: ~s with env: ~s" expr env)
  (cond [(immediate? expr) (emit-immediate expr)]
        [(if? expr)        (emit-if si env expr)]
        [(and? expr)       (emit-if si env (transform-and expr))]
        [(or? expr)        (emit-if si env (transform-or expr))]
        [(primcall? expr)  (emit-primcall si env expr)]
        [(variable? expr)  (emit-variable-ref env expr)]
        [(let? expr)       (emit-let si env expr)]
        [(letrec? expr)    (emit-letrec si env expr)]
        [(app? expr)       (if (equal? 'app (car expr))
                               (error 'emit-expr "encountered 'app'")
                               (emit-app si env expr))]
        [else
         (error 'emit-expr (format "Cannot encode this peanut: ~s" expr))]))

(define (emit-function-header name)
  (emit "global ~a" name)
  (emit "~a:" name))

(define (emit-program expr)
  (set! user-lambdas '()) ; clear from previous invocations
  (emit-function-header "L_scheme_entry")
  (emit-expr 0 '() expr)
  (emit "  ret")
  (emit-function-header "scheme_entry")
  (emit "  mov rcx, rsp")
  (emit "  mov rsp, rdi")
  (emit "  call L_scheme_entry")
  (emit "  mov rsp, rcx")
  (emit "  ret")
  (for-each (lambda (emitter) (emitter))
            user-lambdas))
