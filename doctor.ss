#lang scheme/base
(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (change-person phrase)
  (map (lambda (x) (replace '((are am) (me you) (am are) (my your) (you i) (i you) (are am) (your my)) x)) phrase))


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

(define (fifty-fifty)
  (= (random 2) 0))

(define (word-is-in-the-list? lst word)
  (cond ((null? lst) #f)
        ((equal? (car lst) word) #t)
        (else (word-is-in-the-list? (cdr lst) word))))

(define (phrase-is-in-the-list? lst phrase result)
  (if (null? phrase)
      result
      (phrase-is-in-the-list? lst (cdr phrase) (or result (word-is-in-the-list? lst (car phrase))))))

(define (get-words lst phrase)
  (if (null? lst)
      '()
      (append (filter
               (lambda (x) (word-is-in-the-list? (car (car lst)) x)) phrase)
              (get-words (cdr lst) phrase))))

(define (get-phrase lst word)
  (cond ((null? lst)
         '())
        ((word-is-in-the-list? (car (car lst)) word)
         (pick-random (cadr (car lst))))
        (else (get-phrase (cdr lst) word))))
      


(define (family-or-depressed2 lst phrase)
  (let ((possible-words (get-words lst phrase)))
    (if (null? possible-words)
        (hedge)
        (get-phrase lst (pick-random (filter (lambda (x) (not (null? x))) possible-words))))))



(define (reply usr-resp prev-lst predicates-procs)
  (let ((possible-variants (map (lambda (x)
                                  (if ((cadr x) usr-resp prev-lst)
                                      (list (car x) (caddr x))
                                      (list)))
                                predicates-procs)))
    ((cadr (pick-random-by-weight2 (sum-weight (filter (lambda (x) (not (null? x))) possible-variants) 0)
                                   (filter (lambda (x) (not (null? x))) possible-variants))) usr-resp prev-lst)))
           
(define (ask-patient-name);процедура считывания имени
  (newline)
  (print '(next!))
  (newline)
  (print '(who are you?))
  (newline)
  (print '**)
  (car (read)))

(define (print-goodbye name)
  (print (list 'goodbye name))
  (print '(see you next week))
  (visit-doctor))

(define (pick-random-by-weight lst)
  (let ((max-weight (max lst)))
    (pick-random (filter (lambda (x) (= (car x) (car max-weight))) lst))))

(define (sum-weight lst result)
  (if (null? lst)
      result
      (sum-weight (cdr lst) (+ (car (car lst)) result))))

(define (pick-random-by-weight2 sum lst)
  (let ((rnd (random sum)))
    (cond ((null? (cdr lst))
           (car lst))
          ((and (> (sum-weight lst 0) rnd) (< (sum-weight (cdr lst) 0) rnd))
           (car lst))
          (else (pick-random-by-weight2 sum (cdr lst))))))

(define (max lst)
  (let ((y (car (car lst))))
    (if (null? (cdr lst))
        (car lst)
        (let ((m (max (cdr lst))))
          (if (> (car m) y)
              m
              (car lst))))))


(define (print-reply name user-resp prev-lst)
  ;(print (reply-old user-resp prev-lst))
  (print (reply user-resp
                prev-lst
                (list
                  (list 2 (lambda (ur pl) (< (length ur) 3)) (lambda (ur pl) '(Could you say more?)))
                  (list 1 (lambda (ur pl) #t) (lambda (ur pl) (append (qualifier) (change-person ur))))
                  (list 1 (lambda (ur pl) #t) (lambda (ur pl) (hedge)))
                  (list 4 (lambda (ur pl) (not (null? pl))) (lambda (ur pl)
                                                            (append '(earlier you said)
                                                                    (change-person (pick-random pl)))))
                  (list 2 (lambda (ur pl) #t) (lambda (ur pl)
                                              (family-or-depressed2 '(
                                                                     ((depressed suicide)
                                                                      ((when you feel depressed, go out for the scream)
                                                                       (depression is a disease that can be treated)))
                                                                     ((mother father family parents mom dad)
                                                                      ((tell me more about your family)
                                                                       (why do you feel that way about your parents?))))
                                                                   ur)))))) 
  (doctor-driver-loop name (cons user-resp prev-lst)))

(define (doctor-driver-loop name prev-lst)
  (newline)
  (print '**)
  (let ((user-response (read)))
    (if (equal? user-response '(goodbye))
                (print-goodbye name)
                (print-reply name user-response prev-lst))))

(define (visit-doctor)
  (let ((name (ask-patient-name)))
    (cond ((equal? name 'supertime)
           (print '(it is time to go home)))
          (else 
           (print (list 'hello name))
           (print '(what seems to be the trouble?))
           (doctor-driver-loop name '())))))
(visit-doctor)