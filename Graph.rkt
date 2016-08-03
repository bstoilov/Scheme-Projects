#lang racket

;Define the empty graph 
(define graph '())

(define (split-path path)
(regexp-split #rx"/" path))

(define (contains? lst element)
  (cond ((empty? lst) #f)
        ((equal? (car lst) element) #t)
        (else (contains? (cdr lst) element))))

; Graph mutator. All modify operations use this procedure 
(define (modify-graph newGraph)
 (set! graph newGraph)
  graph)

; Adds a node to the graph ( name '(chidren) )  (add-node 1 '(2 3))
(define (add-node name children)
  (define (add-node-helper name children graph)
  (cons (list name children) graph))
  (modify-graph (add-node-helper name children graph)))

(define (add-children children)
(cond ((empty? children) "" )
  (else (add-node (car children) '()) (add-children (cdr children))))
  )

(define (add name children)
  (add-node name children)
  (add-children children))

; Generic procedure which removes elements from list that match certain condition
(define (remove-element elements condition?)
  (cond ((empty? elements) elements)
        ((condition? (car elements)) (remove-element (cdr elements) condition?))
        (else (cons (car elements) (remove-element (cdr elements) condition? ))))
  )


; Removes a node, and all references to it.
(define (remove name)
  (define (remove-node name)
  (define (condition? element)
    (equal? (car element) name))
  (remove-element graph condition?))

  (define (remove-all-refs name)
    (define (remove-child name node)
      (define (condition? element)
        (eq? name element))
      (cons (car node) (list (remove-element (cadr node) condition?))))
    
    (define (remove-all-refs-helper name graph)
      (cond ((empty? graph) graph)
            (else (cons (remove-child name (car graph))  (remove-all-refs-helper name (cdr graph))))))
  
  (remove-all-refs-helper name graph))
  
  (modify-graph (remove-node name))
  (modify-graph (remove-all-refs name))  
 )
 


(define (get-file name)
  (define (get-file-helper system)
    (cond ((empty? system) -1)
          ((equal? (caar system) name) (car system)) 
          (else (get-file-helper (cdr system)))
          ))
  
  (get-file-helper graph))

(define (has-child? file child)

  (contains? (cadr file) child))

(define (get-file-path path)
  (define (helper pathList result)
    (cond ((empty? pathList) result)
          ((has-child? result (car pathList)) (helper (cdr pathList) (get-file (car pathList))))
          (else -3)))
  (helper (split-path path) (get-file "root"))
  )
  

(add "root" '("dir1" "dir2" "dir3"))
(add "dir2" (list (cons "file" "contents") "folder1" "folder2"))


(define root (get-file "root"))
(define dir2 (get-file "dir2"))