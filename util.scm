(define (empty? lst)
  (eq? lst '()))


(define colours
  (list
   "\033[0m"
   "\033[36m"
   "\033[31m"
   "\033[32m"
   "\033[33m"
   "\033[34m"
   "\033[35m"
   "\033[36m"
   "\033[30;1;41m"
   "\033[30;1;42m"
   "\033[30;1;44m"
   "\033[30;1;45m"
   "\033[30;1;46m"
   "\033[30;1;100m"
   "\033[30;1;101m"
   "\033[30;1;102m"
   "\033[30;1;103m"
   "\033[30;1;104m"
   "\033[30;1;105m"
   "\033[30;1;106m"
   ))

(define (random-element list)
  (list-ref list (random (length list))))



(define (take n lst)
  (define (impl n lst acc)
    (if (or (null? lst) (eq? n 0))
        (reverse acc)
        (impl (sub1 n)
              (cdr lst)
              (cons (car lst) acc))))
  (impl n lst '()))

(define (drop n lst)
  (if (or (null? lst) (eq? n 0))
      lst
      (drop (sub1 n) (cdr lst))))

(define (split-at n lst)
  (define (impl n lst acc)
    (if (or (null? lst) (eq? n 0))
        (list (reverse acc)
              lst)
        (impl (sub1 n)
              (cdr lst)
              (cons (car lst) acc))))
  (impl n lst '()))

(define (partition-all n lst)
  (define (impl lst acc)
    (if (null? lst)
        (reverse acc)
        (let ([s (split-at n lst)])
          (impl (cadr s) (cons (car s) acc)))))
  (impl lst '()))

(define (printf str . args)
  (apply fprintf (console-output-port) str args))
