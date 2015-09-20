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
  (many-replace2 '((are am) (me you) (am are) (my your) (you i) (are am) (your my)) phrase))

(define (many-replace2 replacement-pairs lst) ;переделанная процедура для замены слов во фразе
  (cond ((null? lst) '())
        (else 
         (cons (replace2 replacement-pairs (car lst)) (many-replace2 replacement-pairs (cdr lst))))))

(define (replace2 replacement-pairs word) ;переделанная процедура для замены слов
  (cond ((null? replacement-pairs) word)
        ((equal? (car (car replacement-pairs)) word) (cadr (car replacement-pairs)))
        (else (replace2 (cdr replacement-pairs) word))))
  

(define (qualifier)
  (pick-random '((you seem to think)
                 (you feel that)
                 (why do you believe)
                 (why do you say)
                 (why do you think problem is that)
                 (when did you explore that))))
(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue)
                 (it is very widespread problem)
                 (i understand))))

;(define (fifty-fifty)
;  (= (random 2) 0))

(define (prob) (random 3))

(define (reply user-response previous-lst)
  (let ((rndm (prob)))
    (cond ((and (= rndm 0) (not (null? previous-lst)))
           (append '(earlier you said) (pick-random previous-lst)))
          ((= rndm 1)
           (append (qualifier)
                   (change-person user-response)))
          (else (hedge)))))
           
  
 ; (cond ((fifty-fifty)
  ;       (append (qualifier)
  ;               (change-person user-response)))
  ;      (else (hedge))))

(define (ask-patient-name)
  (newline)
  (print '(next!))
  (newline)
  (print '(who are you?))
  (car (read)))


(define (doctor-driver-loop name prev-lst)
  (newline)
  (print '**)
  (let ((user-response (read)))
    (let ((reply-sentence (reply user-response prev-lst)))
      (cond ((equal? user-response '(goodbye))
         (print (list 'goodbye name))
         (print '(see you next week))
         (visit-doctor))
        (else
         (print reply-sentence)
              (doctor-driver-loop name (cons reply-sentence prev-lst)))))))

(define (visit-doctor)
  (let ((name (ask-patient-name)))
    (cond ((equal? name 'supertime)
           (print '(it is time to go home)))
          (else 
           (print (list 'hello name))
           (print '(what seems to be the trouble?))
           (doctor-driver-loop name '())))))
(visit-doctor)