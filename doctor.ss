#lang scheme/base
(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (many-replace replacement-pairs lst)
  (cond ((null? replacement-pairs) lst)
        (else (let ((pat-rep (car replacement-pairs)))
                (replace (car pat-rep)
                         (cadr pat-rep)
                         (many-replace (cdr replacement-pairs) lst))))))

(define (replace pattern replacement lst)
  (cond ((null? lst) '())
        ((equal? (car lst) pattern)
         (cons replacement
               (replace pattern replacement (cdr lst))))
        (else
         (cons (car lst)
               (replace pattern replacement (cdr lst))))))

(define (change-person phrase)
  (many-replace '((you i) (are am) (your my) (i you) (me you) (am are) (my your)) phrase))



(define (qualifier)
  (pick-random '((you seem to think)
                 (you feel that)
                 (why do you believe)
                 (why do you say))))
(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue))))

(define (fifty-fifty)
  (= (random 2) 0))

(define (reply user-response)
  (cond ((fifty-fifty)
         (append (qualifier)
                 (change-person user-response)))
        (else (hedge))))
(define (doctor-driver-loop name)
  (newline)
  (print '**)
  (let ((user-response (read)))
  (cond ((equal? user-response '(goodbye))
         (print (list 'goodbye name))
         (print '(see you next week)))
        (else (print (reply user-response))
              (doctor-driver-loop name)))))

(define (visit-doctor name)
  (print (list 'hello name))
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name))
