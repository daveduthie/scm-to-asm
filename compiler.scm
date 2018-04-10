(load "util.scm")

(load "tests-1.9-req.scm")
(load "tests-1.8-req.scm")
(load "tests-1.7-req.scm")
(load "tests-1.6-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")

;;;; Helpers ----------------------------------------------------------------

(define wordsize 8)
(define fixnum-shift                      2)
(define fixnum-tag                     #b00)
(define fixnum-mask                    #b11)
(define char-shift                        8)
(define char-tag                 #b00001111)
(define char-mask                #b11111111)
(define bool-shift                        7)
(define bool-t                   #b10011111)
(define bool-f                   #b00011111)
(define bool-tag                  #b0011111)
(define bool-mask                 #b1111111)
(define empty-list               #b00101111)
(define nil                      empty-list)
(define obj-mask                      #b111)
(define pair-tag                      #b001)
(define car-offset             (- pair-tag))
(define cdr-offset    (- wordsize pair-tag))
(define vector-tag                    #b101)
(define string-tag                    #b110)

(define (emit . args)
  (display (apply format args))
  (newline))

(define (last l)
  (car (reverse l)))

(define (butlast l)
  (cdr (reverse l)))

(define reg-ret "rax") ; return register
(define reg-sp "rsp") ; stack base
(define reg-bp "rbp") ; heap base
(define reg-bx "rbx") ; TODO: used for what?
(define reg-ret-bit1 "al") ; low bit of return reg
(define reg-ctx "rcx")

(define (emit-bin-op operator opa opb)
  (emit "  ~a ~a, ~a" operator opa opb))

(define (emit-un-op operator op)
  (emit "  ~a ~a" operator op))

(define (emit-mov dest src)
  (emit "  mov ~a, ~a" dest src))

(define (emit-ret<- op . args)
  (emit "  mov rax, ~a" (apply format op args)))

(define (emit-ret-> op . args)
  (emit "mov ~a, rax" (apply format op args)))

(define (emit-val val)
  (emit "  mov rax, ~s" val))

(define (emit-return)
  (emit "  ret"))

(define (emit-stack-save si)
  (emit "  mov qword [rsp - ~s], rax" si))

(define (emit-stack-load si)
  (emit "  mov rax, [rsp - ~s]" si))

;; TODO: this abstraction sucks!
(define (emit-add-stack-offset offset)
  (emit "  add rax, [rsp - ~s]" offset))

(define (emit-heap-save offset)
  (emit "  mov [rbp - ~a], rax" offset))

(define (emit-heap-load offset)
  (emit "  mov rax, [rbp - ~a]" offset))

(define (emit-label lbl)
  (emit "~a:" lbl))

(define (reg+offset reg off)
  (format "[~a + ~a]" reg off))

(define (reg-offset reg off)
  (format "[~a - ~a]" reg off))

(define (reg-loc reg)
  (format "[~a]" reg))

;;;; Immediate values ----------------------------------------------------------------

(define fxbits (- (* wordsize 8) fixnum-shift))
(define fxlower (- (expt 2 (- fxbits 1))))
(define fxupper (sub1 (expt 2 (- fxbits 1))))
(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (equal? x '())
      (equal? x 'nil)))

(define (immediate-rep x)
  (cond
   [(fixnum? x)    (ash x fixnum-shift)]
   [(boolean? x)   (if x bool-t bool-f)]
   [(equal? x '()) empty-list]
   [(equal? x 'nil) empty-list]
   [(char? x)
    (bitwise-ior (ash (char->integer x) char-shift) char-tag)]
   [else
    (error 'emit-program
           "Cannot find representation for this element: " x)]))

(define (emit-immediate tail? x)
  (emit-val (immediate-rep x))
  (if tail? (emit-return)))

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
    (if tail? (emit-return))))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env #f arg)
  (emit-bin-op 'add reg-ret (immediate-rep 1)))

(define-primitive (fxsub1 si env arg)
  (emit-expr si env #f arg)
  (emit-bin-op 'sub reg-ret (immediate-rep 1)))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env #f arg)
  (emit-bin-op 'shl reg-ret (- char-shift fixnum-shift))
  (emit-bin-op 'or reg-ret char-tag))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env #f arg)
  (emit-bin-op 'shr reg-ret char-shift)
  (emit-bin-op 'shl reg-ret fixnum-shift))

(define-primitive (not si env arg)
  (emit-if si env #f (list 'if arg #f #t)))

(define-primitive (fxlognot si env arg)
  (emit-expr si env #f arg)
  (emit-bin-op 'or reg-ret fixnum-mask)
  (emit-un-op 'not reg-ret))

(define-primitive (car si env arg)
  (emit-expr si env #f arg)
  (emit-ret<- (reg-offset reg-ret pair-tag)))

(define-primitive (cdr si env arg)
  (emit-expr si env #f arg)
  (emit-ret<- (reg+offset reg-ret (- wordsize pair-tag))))

(define-primitive (cons si env left right)
  ;; emit cdr
  (emit-expr si env #f left)
  (emit-ret-> (reg-loc reg-bp))
  ;; emit car
  (emit-expr si env #f right)
  (emit-ret-> (reg+offset reg-bp wordsize))
  ;; move car's address into RAX
  (emit-ret<- reg-bp)
  (emit-bin-op 'or reg-ret pair-tag)
  (emit-bin-op 'add reg-bp (* wordsize 2)))

(define-primitive (set-car! si env target val)
  (emit-expr si env #f target)
  (emit-bin-op 'mov reg-bx reg-ret)
  (emit-expr si env #f val)
  (emit-ret-> (reg+offset reg-bx car-offset)))

(define-primitive (set-cdr! si env target val)
  (emit-expr si env #f target)
  (emit-ret-> reg-bx)
  (emit-expr si env #f val)
  (emit-ret-> (reg+offset reg-bx cdr-offset)))

;;; Need to keep track of:
;;;   - initial RBP si + w
;;;   - init expression si + 3w
;;;   - len si + 2w
;;;
(define-primitive (make-vector si env len init)
  (let ([top-label   (unique-label "loop_top")]
        [entry-label (unique-label "loop_entry")]
        [rbp-pos     (+ si wordsize)]
        [len-pos     (+ si (* 2 wordsize))]
        [init-pos    (+ si (* 3 wordsize))])
    (emit "  ;; making vector ~s ~s" len init)

    ;; save RBP for return
    (emit-mov (reg-offset reg-sp rbp-pos)
              reg-bp)

    ;; evaluate len expression
    (emit-expr len-pos env #f len)
    (emit-stack-save len-pos)

    ;; evaluate init expression
    (emit-expr init-pos env #f init) ; now init is in RAX

    ;; push vector-len into first position
    (emit-mov reg-bx
              (reg-offset reg-sp len-pos))
    (emit-mov (reg-loc reg-bp)
              reg-bx)
    (emit-bin-op 'shr reg-bx fixnum-shift)
    (emit-bin-op 'add reg-bp wordsize)

    ;;; place init expr into each slot in vector
    ;; TODO: abstract out looping
    (emit-un-op 'jmp entry-label)
    (emit-label top-label)
    (emit-heap-save 0)
    (emit-bin-op 'add reg-bp wordsize)
    (emit-bin-op 'sub reg-bx 1)
    (emit-label entry-label)
    (emit-bin-op 'cmp reg-bx 0)
    (emit-un-op 'jne top-label)

    ;; return vector-tagged address
    (emit-stack-load rbp-pos)
    (emit-bin-op 'or reg-ret vector-tag)
    (emit "  ;; finished making vector ~s ~s" len init)))

(define-primitive (vector-length si env vec)
  (emit-expr si env #f vec)
  (emit-mov reg-ret (reg-offset reg-ret vector-tag)))

(define-primitive (vector-ref-unsafe si env vec idx)
  (let ([idx-pos (+ si wordsize)]
        [vec-pos (+ si (* 2 wordsize))])

    ;; TODO: what is going on here?
    ;; try duplicating some of these instructions
    ;; and be amazed
    (emit-expr idx-pos env #f idx)
    (emit-bin-op 'shr reg-ret fixnum-shift)
    ;; (emit-bin-op 'shr reg-ret fixnum-shift)
    ;; ...
    (emit-bin-op 'imul reg-ret wordsize)
    (emit-stack-save idx-pos)

    (emit-expr vec-pos env #f vec)
    (emit-bin-op 'add reg-ret (- wordsize vector-tag))
    (emit-add-stack-offset idx-pos)
    ;; grab `idx`'th element
    (emit-mov reg-ret (reg-loc reg-ret))))

(define-primitive (vector-set-unsafe! si env vec idx elem)
  (let ([idx-pos  (+ si wordsize)]
        [elem-pos (+ si (* 2 wordsize))]
        [vec-pos  (+ si (* 3 wordsize))])

    (emit-expr idx-pos env #f idx)
    (emit-bin-op 'shr reg-ret fixnum-shift)
    (emit-bin-op 'imul reg-ret wordsize)
    (emit-stack-save idx-pos)

    (emit-expr elem-pos env #f elem)
    (emit-stack-save elem-pos)

    (emit-expr vec-pos env #f vec)
    (emit-bin-op 'add reg-ret (- wordsize vector-tag))
    (emit-add-stack-offset idx-pos)
    (emit-mov reg-bx (reg-offset reg-sp elem-pos))
    (emit-mov (reg-loc reg-ret) reg-bx)))

(define-primitive (vector-ref si env vec idx)
  (emit-let
   si
   env
   #f
   `(let [v ,vec]
      (if (fx< ,idx (vector-length v))
          (vector-ref-unsafe v ,idx)
          nil))))

(define-primitive (vector-set! si env vec idx elem)
  (emit-let
   si
   env
   #f
   `(let [v ,vec
          i ,idx]
      (if (fx< i (vector-length v))
          (vector-set-unsafe! v i ,elem)
          nil))))

(define-primitive (begin si env . args)
  (let ([exprs (butlast args)]
        [ret   (last args)])
    (for-each (lambda (e) (emit-expr si env #f e)) exprs)
    (emit-expr si env #f ret)))

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
       (emit-un-op 'setz reg-ret-bit1)
       (emit-bin-op 'movzx reg-ret reg-ret-bit1)
       (emit-bin-op 'shl reg-ret bool-shift)
       (emit-bin-op 'or reg-ret bool-tag))]))

(define-predicate (fixnum? si env arg)
  (emit-bin-op 'and reg-ret fixnum-mask))

(define-predicate (fxzero? si env arg)
  (emit-bin-op 'cmp reg-ret fixnum-tag))

(define-predicate (null? si env arg)
  (emit-bin-op 'cmp reg-ret empty-list))

(define-predicate (boolean? si env arg)
  (emit-bin-op 'and reg-ret bool-mask)
  (emit-bin-op 'cmp reg-ret bool-tag))

(define-predicate (char? si env arg)
  (emit-bin-op 'and reg-ret char-mask)
  (emit-bin-op 'cmp reg-ret char-tag))

(define-predicate (pair? si env arg)
  (emit-bin-op 'and reg-ret obj-mask)
  (emit-bin-op 'cmp reg-ret pair-tag))

(define-predicate (vector? si env arg)
  (emit-bin-op 'and reg-ret obj-mask)
  (emit-bin-op 'cmp reg-ret vector-tag))

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
    (emit-bin-op 'cmp reg-ret bool-f)
    (emit-un-op 'je alt-label)
    (emit-bin-op 'cmp reg-ret nil)
    (emit-un-op 'je alt-label)
    (emit-expr si env tail? consequent)
    (emit-un-op 'jmp end-label)
    (emit-label alt-label)
    (emit-expr si env tail? alternate)
    (emit-label end-label)))

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

(define (or? expr)
  (and (pair? expr) (equal? (car expr) 'or)))

(define (transform-or expr)
  (let alternate ([i (cdr expr)])
    (if (null? i)
        #f
        `(if ,(car i) #t ,(alternate (cdr i))))))

;;;; Higher-arity primitives ----------------------------------------------------------------

(define (build-on-stack si env args)
  (let loop ([offset (+ si wordsize)] [args args])
    (unless (null? args)
      (emit-expr offset env #f (car args))
      (unless (null? (cdr args))
        (emit-mov (reg-offset reg-sp offset)
                  reg-ret))
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
          [(eq? len 0) (emit-mov reg-ret (immediate-rep init))]
          [(eq? len 1) (emit-expr si env #t (car args))]
          [else        (begin
                         (emit "  ; emitting build on stack (si: ~s)" si)
                         (build-on-stack si env rev)
                         (reduce-stack si env (sub1 len) rf nil))])))]))

(define-primitive-reduction (fx+ si env . args)
  (lambda (si env fail)
    (emit-bin-op 'add reg-ret (reg-offset reg-sp si)))
  0)

(define-primitive-reduction (fx- si env . args)
  (lambda (si env fail)
    (emit-bin-op 'sub reg-ret (reg-offset reg-sp si)))
  0)

(define-primitive-reduction (fx* si env . args)
  (lambda (si env fail)
    (emit-bin-op 'shr reg-ret fixnum-shift)
    (emit-bin-op 'imul reg-ret (reg-offset reg-sp si)))
  1)

(define-primitive-reduction (fxlogor si env . args)
  (lambda (si env fail)
    (emit-bin-op 'or reg-ret (reg-offset reg-sp si)))
  0)

(define-primitive-reduction (fxlogand si env . args)
  (lambda (si env fail)
    (emit-bin-op 'and reg-ret (reg-offset reg-sp si)))
  0)

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
                         (emit-mov reg-ret bool-t)
                         (emit-un-op 'jmp end)
                         (emit-label fail)
                         (emit-mov reg-ret bool-f)
                         (emit-label end))])))]))

(define-primitive-variadic-predicate (fx= si env . args)
  (lambda (si env fail)
    (emit-bin-op 'cmp reg-ret (reg-offset reg-sp si))
    (emit-un-op 'jne fail)))

(define-primitive-variadic-predicate (eq? si env . args)
  (lambda (si env fail)
    (emit-bin-op 'cmp reg-ret (reg-offset reg-sp si))
    (emit-un-op 'jne fail)))

(define-primitive-variadic-predicate (fx< si env . args)
  (lambda (si env fail)
    (emit-bin-op 'cmp reg-ret (reg-offset reg-sp si))
    (emit-un-op 'jnl fail)))

(define-primitive-variadic-predicate (fx<= si env . args)
  (lambda (si env fail)
    (emit-bin-op 'cmp reg-ret (reg-offset reg-sp si))
    (emit-un-op 'jnle fail)))

(define-primitive-variadic-predicate (fx> si env . args)
  (lambda (si env fail)
    (emit-bin-op 'cmp reg-ret (reg-offset reg-sp si))
    (emit-un-op 'jng fail)))

(define-primitive-variadic-predicate (fx>= si env . args)
  (lambda (si env fail)
    (emit-bin-op 'cmp reg-ret (reg-offset reg-sp si))
    (emit-un-op 'jnge fail)))

;;;; Let bindings ----------------------------------------------------------------
;;; Can't I desugar these into lambdas?
;;; Oh, I don't have lambdas yet :P

(define (let? expr)
  (and (pair? expr) (equal? (car expr) 'let)))

(define (extend-env sym si env)
  (cons (cons sym si) env))

(define (emit-let si env tail? expr)
  (define (process-let bindings si new-env)
    (if (empty? bindings)
        (let ([tail-expr (car (reverse (cddr expr)))])
          (for-each (lambda (exp) (if (equal? exp tail-expr) ; TODO: I'm ugly
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

(variable? 'foo)

(define (lookup sym env)
  (cdr (assoc sym env)))

(define (emit-variable-ref env tail? sym)
  (cond [(lookup sym env) => emit-stack-load]
        [else (errorf 'emit-variable-ref "Symbol: ~s is unbound" sym)])
  (if tail? (emit-return)))

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
        (emit-stack-load (+ offset shift))
        (emit-stack-save offset)
        (loop (sub1 x) (+ offset wordsize))))))

(define (emit-app si env tail? expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env #f (car args))
      (emit-stack-save si)
      (emit-arguments (+ si wordsize) (cdr args))))
  (let* ([args (cdr expr)]
         [base (+ si (* 2 wordsize))])
    (emit "  ; --------------------------------- emitting args before app call <<*>>")
    (emit-arguments base args)
    (if tail?
        (begin (shift-stack-frame-down si (length args))
               (emit-un-op 'jmp (lookup (car expr) env)))
        (begin (emit-bin-op 'sub reg-sp si)
               (emit-un-op 'call (lookup (car expr) env))
               (emit-bin-op 'add reg-sp si)))))

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
                        (emit-mov (reg+offset reg-ctx num) name))))

(define (restore-registers)
  (preserve-registers (lambda (name num)
                        (emit-mov name (reg+offset reg-ctx num)))))

;;;; Compiler ----------------------------------------------------------------

(define (emit-expr si env tail? expr)
  ;; TODO: rewrite with pattern-matching
  ;; (printf "EMIT: ~s~%" expr)
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
  (emit-label name))

(define (emit-program expr)
  (set! user-lambdas '()) ; clear from previous invocations
  (emit-function-header 'L_scheme_entry)
  (emit-expr 0 '() #t expr)
  (emit-function-header "scheme_entry")
  (emit "  ; expect rdi <- &ctx,")
  (emit "  ; rsi <- stack_base, rdx <- heap_base")
  (emit-mov reg-ctx 'rdi) ;<<< first arg
  (backup-registers)
  (emit-mov reg-sp 'rsi) ;<<< second arg
  (emit-mov reg-bp 'rdx) ;<<< third arg
  (emit-un-op 'call 'L_scheme_entry)
  (restore-registers)
  (emit-return)
  (for-each (lambda (emitter) (emitter))
            user-lambdas))
