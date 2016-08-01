#lang racket

;Define the empty graph 
(define graph '())

; Graph mutator. All modify operations use this procedure 
(define (modify-graph newGraph)
 (set! graph newGraph)
  graph)

; Adds a node to the graph ( name '(chidren) )  (add-node 1 '(2 3))
(define (add-node name children)
  (define (add-node-helper name children graph)
  (cons (list name children) graph))
  (modify-graph (add-node-helper name children graph)))

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
    (eq? (car element) name))
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
 


(add-node 1 '(2 3))
(add-node 3 '(5 6))
(add-node 2 '(7 8 9 10 11))
