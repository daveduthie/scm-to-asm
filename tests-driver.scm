(load "util.scm")

(define cached-asm (make-eqv-hashtable 100))

(define all-tests '())

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string] ...)
     (set! all-tests
        (cons
           '(test-name [expr string output-string] ...)
            all-tests))]))

(define (build)
  (unless (zero? (system "make"))
    (error 'build "could not build target")))

(define (test-one test-id test)
  (let ([expr (car test)]
        [type (cadr test)]
        [out  (caddr test)])
    (printf (random-element colours))
    (printf "test ~s:~s ..." test-id expr)
    (printf "\033[0m")
    (flush-output-port)
    (case type
     [(string) (test-with-string-output test-id expr out)]
     [else (error 'test-one "invalid test type" type)])
    (printf " ok\n")))

(define (test-all)
  (let f ([i 0] [ls (reverse all-tests)])
    (if (null? ls)
        (printf "passed all ~s tests\n" i)
        (let ([x (car ls)] [ls (cdr ls)])
          (let* ([test-name (car x)]
                 [tests (cdr x)]
                 [n (length tests)])
            (printf "Performing ~a tests ...\n" test-name)
            (let g ([i i] [tests tests])
              (cond
                [(null? tests) (f i ls)]
                [else
                 (test-one i (car tests))
                 (g (add1 i) (cdr tests))])))))))


(define input-filter 
  (make-parameter (lambda (x) x)
    (lambda (x)
      (unless (procedure? x)
        (error 'input-filter "not a procedure" x))
      x)))

(define runtime-file
  (make-parameter
    "runtime.c"
    (lambda (fname)
      (unless (string? fname) (error 'runtime-file "not a string" fname))
      fname)))


(define compile-port
  (make-parameter
    (current-output-port)
    (lambda (p)
       (unless (output-port? p)
         (errorf 'compile-port "not an output port ~s" p))
       p)))

(define show-compiler-output (make-parameter #f))

(define (run-compile expr)
  (with-output-to-string
    (lambda ()
      (emit-program expr))))

(define (write-to-asm-file asm)
  (let ([p (open-output-file "stst.s" 'replace)])
    (fprintf p asm)
    (close-output-port p)))


(define (execute)
  (unless (fxzero? (system "./stst > stst.out"))
    (error 'execute "produced program exited abnormally")))

(define (get-string file-name)
  (with-output-to-string
    (lambda ()
      (with-input-from-file file-name
        (lambda ()
          (let f ()
            (let ([c (read-char)])
              (cond
               [(eof-object? c) (void)]
               [else (display c) (f)]))))))))

(define (test-with-string-output test-id expr expected-output)
  (let [(asm (run-compile expr))
        (cached (hashtable-ref cached-asm test-id ""))]
    (unless (string=? asm cached)
      (write-to-asm-file asm) ; write to stst.s
      (build)                 ; assemble, compile, link
      (execute)               ; ./stst > stst.out
      (let [(output (get-string "stst.out"))]
        (if (string=? expected-output output)
            (hashtable-set! cached-asm test-id asm)
            (error 'test (format "Failed test ~a ~%expected: ~s ~%got ~s"
                                 test-id expected-output output)))))))


(define (retest?)
  (let [(r (get-string "retest"))]
    (case r
      ["true\n" (begin (system "./retest-false") #t)]
      ["false\n" #f]
      [else (error 'retest? "Halting retest loop" r)])))

(define sleep-duration (make-time 'time-duration 0 1))

(define (test-all-loop)
  (sleep sleep-duration)
  (if (retest?)
      (begin (set! all-tests '())
             (load "compiler.scm")
             (test-all)))

  (test-all-loop))

(define (handle-stuff)
  (guard (exn
          ((condition? exn)
           (newline)
           (if (message-condition? exn)
               (printf (condition-message exn))
               (printf "Ouch!"))
           (newline)
           (printf (random-element colours))
           (display (current-time))
           (printf "\033[0m")
           (newline)
           (handle-stuff)))
    (test-all-loop)))
