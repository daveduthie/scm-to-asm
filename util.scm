;; (define (myerror msg . args)
;;   (raise
;;    (make-message-condition (apply format msg args))))


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



