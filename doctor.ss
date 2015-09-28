#lang scheme/base
(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (change-person phrase)
  (map (lambda (x) (replace '((are am) (me you) (am are) (my your) (you i) (are am) (your my)) x)) phrase))


(define (replace replacement-pairs word) ;переделанная процедура для замены слов
  (cond ((null? replacement-pairs) word)
        ((equal? (car (car replacement-pairs)) word) (cadr (car replacement-pairs)))
        (else (replace (cdr replacement-pairs) word))))

(define (qualifier)
 (append (pick-random '((why do you)
                        (when did you)
                        (you seem to)))
         (pick-random '((think problem is that)
                        (say)
                        (believe)
                        (think)))))
(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue)
                 (it is very widespread problem)
                 (i understand)
                 (what the problem?))))

(define (prob) (random 4))

(define (word-is-in-the-list? lst word)
  (cond ((null? lst) #f)
        ((equal? (car lst) word) #t)
        (else (word-is-in-the-list? (cdr lst) word))))

(define (phrase-is-in-the-list? lst phrase result)
  (if (null? phrase)
      result
      (phrase-is-in-the-list? lst (cdr phrase) (or result (word-is-in-the-list? lst (car phrase))))))

(define (family-or-depressed lst phrase)
  (cond ((null? phrase) (hedge))
        ((phrase-is-in-the-list? (car (car lst)) phrase #f) (pick-random (cadr (car lst))))
        (else (family-or-depressed (cdr lst) phrase)))) 

(define (reply user-response previous-lst)
  (let ((rndm (prob)))
    (cond ((and (= rndm 0) (not (null? previous-lst)))
           (append '(earlier you said) (change-person (pick-random previous-lst))))
          ((= rndm 1)
           (append (qualifier)
                   (change-person user-response)))
          ((= rndm 2)
           (family-or-depressed '(
                                  ((depressed suicide)
                                   ((when you feel depressed, go out for the scream)
                                    (depression is a disease that can be treated)))
                                  ((mother father family parents mom dad)
                                   ((tell me more about your *)
                                    (why do you feel that way about your * ?))))
                                           user-response))
          (else (hedge)))))
           
(define (ask-patient-name);процедура считывания имени
  (newline)
  (print '(next!))
  (newline)
  (print '(who are you?))
  (newline)
  (print '**)
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
              (doctor-driver-loop name (cons user-response prev-lst)))))))

(define (visit-doctor)
  (let ((name (ask-patient-name)))
    (cond ((equal? name 'supertime)
           (print '(it is time to go home)))
          (else 
           (print (list 'hello name))
           (print '(what seems to be the trouble?))
           (doctor-driver-loop name '())))))
(visit-doctor)