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

(define (ai-g numList target_num)
  (get-guess-result numList player1_num))

(define (get-guesses)
  (define (count-chars lst char)
    (cond ((empty? lst) 0)
          ((equal? (car lst) char) (+ 1 (count-chars (cdr lst) char)))
          (else (count-chars (cdr lst) char))))
  
  (define (remove-all-with-dubs all)
    (define (num-has-dups? num)
      (define (helper l)
        (cond ((empty? l) #f)
              ((not (equal? 0 (count-chars (cdr l) (car l)))) #t)
              (else (helper (cdr l)))))
      (helper (num_to_list num)))
    
    (cond ((empty? all) '())
          ((num-has-dups? (car all)) (remove-all-with-dubs (cdr all)))
          (else (append (list (car all)) (remove-all-with-dubs (cdr all))))))
  
  (define (gen-all-posible max)
    (define (gen-helper currentNum)
      (cond ((> currentNum max) '())
            (else (append (list currentNum) (gen-helper (+ 1 currentNum))))))
    (gen-helper 1234))
  
  (remove-all-with-dubs (gen-all-posible 9999)))


(define all (get-guesses))
(define player1-guesses all)
(define player2-guesses all)

(define (get-guess)
  (define guess (car all))
  (set! all (cdr all))
  (num_to_list guess))

(define (ai-turn nickname)
  (define guess (get-guess))
  (display nickname)
  (display " made guess ")
  (display guess)
  (newline)
  guess)

(define (ai-player nickname) (ai-turn nickname))
(define player1_num (gen-num))
(define player2_num (gen-num))
(define (p1) (ai-player "AI-1"))
(define (p2) (ai-player "AI-2"))


(define (eval-result resCons)
  (display (car resCons))
  (display " Bulls ")
  (display (cdr resCons))
  (display " Cows")
  (equal? (car resCons) 4))