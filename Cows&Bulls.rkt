#lang racket
(define number_size 4)
(define base_num '(1 2 3 4 5 6 7 8 9))

(define (contains? l i)
  (if (empty? l) #f
      (or (equal? (first l) i) (contains? (rest l) i))))

(define (get-last-n lst n)
  (define (get-last-n-helper l remaining)
    (cond ((= remaining 0) l)
          (else (get-last-n-helper (cdr l) (- remaining 1)))))
  
  (get-last-n-helper lst (- (length lst) n)))

(define (gen-num)
  (get-last-n (shuffle base_num) number_size))

(define (num_to_list num)
  (define (num_to_list-helper num lst)
    (cond ((< num 10) (cons num lst))
          (else (num_to_list-helper (floor (/ num 10)) (cons (modulo num 10) lst)))))
  
  (num_to_list-helper num '()))





(define player1_num (gen-num))
(define player2_num (gen-num))

;1 - Bull, 2 - Cow, 0 - Nothing 
(define (check_index guessNum targetNum index)
  (define (isCow?)
    (contains? targetNum (list-ref guessNum index)))
  (define (isBull?)
    (equal? (list-ref targetNum index) (list-ref guessNum index)))
  
  (cond ((isBull?) 1)
        ((isCow?) 2)
        (else 0)))

(define (get-guess-result num target)
  (define (helper numList index resultCons)
    (cond ((= index (length target)) resultCons)
          ((equal? (check_index numList target index) 1) (helper numList (+ 1 index) (cons (+ 1 (car resultCons)) (cdr resultCons))))
          ((equal? (check_index numList target index) 2) (helper numList (+ 1 index) (cons (car resultCons) (+ 1 (cdr resultCons)))))
          (else (helper numList (+ 1 index) resultCons))))
  (helper num 0 (cons 0 0)))

(define (guess num)
  (define cowBullCons (get-guess-result (num_to_list num) player1_num))
  (display (car cowBullCons))
  (display " Bulls ")
  (display (cdr cowBullCons))
  (display " Cows ")
  (newline)
  (cond ((equal? (car cowBullCons) number_size) (display "Thats correct!"))
        (else (display "Try again") (newline))))

(define (g num)
  (guess num))


(define (ai-g numList)
  (get-guess-result numList player1_num))



(define (handleSide items oldTotal)
  (define (handleCons item)
    (define newTotal (cons-total (ai-g (num_to_list (car item)))))
    (define diff (- newTotal oldTotal))
    (cond ((or (and nineOk  (= diff 0)) (and (not nineOk) (not (= diff 0)))) (set! included (append included (list (cdr item)))))
          (else (set! excluded (append excluded (list (cdr item)))))))

  (define (helper lst)
    (cond ((empty? lst) "")
    (else (handleCons (car lst)) (helper (cdr lst)))))
  (helper items))
  
(define (handleLeft)
 (handleSide leftLst leftTotal))
(define (handleRight)
  (handleSide rightLst rightTotal))








(define nineOk -1)

(define (cons-total c)
  (cond ((pair? c) (+ (car c) (cdr c)))
        (else 0)))




(define leftGuess '())
(define rightGuess '())


(define (determine9)
  (define totalFound (+ (cons-total leftGuess) (cons-total rightGuess)))
  (set! nineOk (not (= totalFound 4))))

(define leftTotal (cons-total leftGuess))
(define rightTotal (cons-total rightGuess))

;FLAGS
(define leftEmpty #t)
(define rightEmpty #t)

(define leftLst (list (cons 9234 1) (cons 1934 2) (cons 1294 3) (cons 1239 4)))
(define rightLst (list (cons 9678 5) (cons 5978 6) (cons 5698 7) (cons 5679 8)))

(define included '())
(define excluded '())



(define (ai)
  (cond (leftEmpty (set! leftEmpty #f) (set! leftTotal (cons-total (ai-g (num_to_list 1234)))) (ai))
        (rightEmpty (set! rightEmpty #f) (set! rightTotal (cons-total (ai-g (num_to_list 5678)))) (ai))
       (else (determine9) (handleLeft) (handleRight))))





;player1_num

player1_num

;(determine_left total)

(ai)
nineOk
included
excluded