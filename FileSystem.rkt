#lang racket

(define file_system (list '/' '())) 


(define (modify-system updated)
  (set! file_system updated)
  )
(define (split-path path)
(regexp-split #rx"/" path))

(define (mkdir path)
  
  (define (mkdir-helper pathList system)

    (cond ((empty? pathList) system)
          ((empty? (cdr pathList)) (list (car system) (cons (car pathList) (mkdir-helper (cdr pathList) (cdr system)))))
          (else (list (car pathList) (mkdir-helper (cdr pathList) (cdr system))))
          ))
  (mkdir-helper (split-path path) file_system)
  
  )



  
(define (mkdirs path)
(define (mkdirs-helper pathList)
 1 
  )
  1
  )




