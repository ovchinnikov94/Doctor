#lang scheme/base

#|
Программа - упрощенная версия медицинской системы "Доктор", разработанной в
Массачусетском Университете в начале 60-х годов.
Автор: Овчинников Дмитрий, гр. 427 каф. СП, ВМК, МГУ (2015 г.)
Выполнены упражнения: 1-6.
|#


#|Функция pick-random.
В качестве параметра принимает список;
результатом является случайный элемент списка |#
(define (pick-random lst)
  (list-ref lst (random (length lst))))


#|Функция change-person.
Параметры: Список (предложение на англ. языке)
Результат: Список (предложение на англ. языке,
с измененными местоимениями с 1го на 2ое лицо и наоборот|#
(define (change-person phrase)
  (map (lambda (x) (replace '((are am) (me you) (am are) (my your) (you i) (i you) (are am) (your my)) x)) phrase))


#|Функция replace.
Параметры: список из пар местоимений, слово
Результат: Если слово имеется в паре первым, то оно меняется на второе|#
(define (replace replacement-pairs word) 
  (cond ((null? replacement-pairs) word)
        ((equal? (car (car replacement-pairs)) word) (cadr (car replacement-pairs)))
        (else (replace (cdr replacement-pairs) word))))


#|Функция qualifier.
Результат: генерирует случайную фразу из 2 частей.|#
(define (qualifier)
 (append (pick-random '((why do you)
                        (when did you)
                        (you seem to)))
         (pick-random '((think problem is that)
                        (say)
                        (believe)
                        (think)))))


#|Функция hedge.
Результат: выдает случайную фразу, из списка возможных. |#
(define (hedge)
  (pick-random '((please go on)
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue)
                 (it is very widespread problem)
                 (i understand)
                 (what the problem?))))


#|Функция word-is-in-the-list?.
Параметры: список слов, одно слово.
Результат: true - если слово имеется в списке и false - иначе|#
(define (word-is-in-the-list? lst word)
  (cond ((null? lst) #f)
        ((equal? (car lst) word) #t)
        (else (word-is-in-the-list? (cdr lst) word))))


#|Функция phrase-is-in-the-list?.
Параметры: список слов, фраза (тоже список слов), результат
Результат: true - если имеется хотя бы одно слово из списка во фразе,
false - иначе|#
(define (phrase-is-in-the-list? lst phrase result)
  (if (null? phrase)
      result
      (phrase-is-in-the-list? lst (cdr phrase) (or result (word-is-in-the-list? lst (car phrase))))))


#|Функция get-words.
Параметры: два списка  lst, phrase
Результат: список - пересечение двух множеств слов.|#
(define (get-words lst phrase)
  (if (null? lst)
      '()
      (append (filter
               (lambda (x) (word-is-in-the-list? (car (car lst)) x)) phrase)
              (get-words (cdr lst) phrase))))


#|Функция get-phrase.
Параметры: список вида (list (list (words) (phrases))), слово.
Результат: генерация случайной фразы из списка, где ключевое слово word|#
(define (get-phrase lst word)
  (cond ((null? lst)
         '())
        ((word-is-in-the-list? (car (car lst)) word)
         (pick-random (cadr (car lst))))
        (else (get-phrase (cdr lst) word))))
      

#|Фунция family-or-depressed.
Параметры: список ключевых слов и возможных ответов, фраза
Результат: генерация ответа на фразу из списка фраз, иначе вызов (hedge)|#
(define (family-or-depressed lst phrase)
  (let ((possible-words (get-words lst phrase)))
    (if (null? possible-words)
        (hedge)
        (get-phrase lst (pick-random (filter (lambda (x) (not (null? x))) possible-words))))))


#|Функция reply.
Параметры: список, реплика пользователя; список - предыдущие реплики пользователя;
список функций и предикатов для них.
Результат: Генерация ответа на реплику, с использованием предикатов и функций, с учетом их веса.|#
(define (reply usr-resp prev-lst predicates-procs)
  (let ((possible-variants (map (lambda (x)
                                  (if ((cadr x) usr-resp prev-lst)
                                      (list (car x) (caddr x))
                                      (list)))
                                predicates-procs)))
    ((cadr (pick-random-by-weight (sum-weight (filter (lambda (x) (not (null? x))) possible-variants) 0)
                                   (filter (lambda (x) (not (null? x))) possible-variants))) usr-resp prev-lst)))


#|Функция ask-patient-name.
Результат: Имя, введенное пользователем на клавиатуре.|#
(define (ask-patient-name)
  (newline)
  (print '(next!))
  (newline)
  (print '(who are you?))
  (newline)
  (print '**)
  (car (read)))

#|Функция print-godbye.
Результат: печатает несколько реплик.|#
(define (print-goodbye name)
  (print (list 'goodbye name))
  (print '(see you next week))
  (visit-doctor))


#|Функция sum-weight.
Параметры: список, где в каждом вложенном списке первый элемент - вес; накопитель результата.
Результат: сумма весов всего списка|#
(define (sum-weight lst result)
  (if (null? lst)
      result
      (sum-weight (cdr lst) (+ (car (car lst)) result))))


#|Функция pick-random-by-weight.
Параметры: сумма весов всего списка, сам список
Результат: случайный(с учетом веса) элемент списка|#
(define (pick-random-by-weight sum lst)
  (let ((rnd (random sum))
        (sumw-lst (sum-weight lst 0)))
    (cond ((null? (cdr lst))
           (car lst))
          ((and (> sumw-lst rnd) (< (- sumw-lst (car (car lst)) 1) rnd))
           (car lst))
          (else (pick-random-by-weight sum (cdr lst))))))

#|Фунция print-reply.
Параметры: имя пользователя, реплика пользователя, список предыдущих реплик.
Результат: распечатка ответа пользователю.ы|#
(define (print-reply name user-resp prev-lst)
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
                                              (family-or-depressed '(
                                                                     ((depressed suicide)
                                                                      ((when you feel depressed, go out for the scream)
                                                                       (depression is a disease that can be treated)))
                                                                     ((mother father family parents mom dad)
                                                                      ((tell me more about your family)
                                                                       (why do you feel that way about your parents?))))
                                                                   ur)))))) 
  (doctor-driver-loop name (cons user-resp prev-lst)))

#|Функция  doctor-driver-loop.
Параметры: имя пользователя, список предыдущих реплик.
Резултат: предложение пользователю начать диалог и разговор с ним пока он не скажет 'goodbye'|#
(define (doctor-driver-loop name prev-lst)
  (newline)
  (print '**)
  (let ((user-response (read)))
    (if (equal? user-response '(goodbye))
                (print-goodbye name)
                (print-reply name user-response prev-lst))))

#|Функция visit-doctor
Основная функция программы.|#
(define (visit-doctor)
  (let ((name (ask-patient-name)))
    (cond ((equal? name 'supertime)
           (print '(it is time to go home)))
          (else 
           (print (list 'hello name))
           (print '(what seems to be the trouble?))
           (doctor-driver-loop name '())))))
(visit-doctor)