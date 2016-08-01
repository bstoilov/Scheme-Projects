#lang racket

(define file_system '())

(define (modify-system updated)
  (set! file_system updated)
  )
(define (split-path path)
(regexp-split #rx"/" path)
  )


(define (list-contains? lst item)
  (cond ((empty? lst) #f)
        ((eq? (car lst) item) #t)
        (else (list-contains? (cdr lst) item))
        )
  )
(define (file-exists? path)
3
  )
(define (mkdir dir)
43

  )


(define string "a/b/c")
(define l '(1 2 3 4 5 "rwef"))
