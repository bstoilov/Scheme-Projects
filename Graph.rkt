#lang racket

 (define (gen-node name children)
   (cons name (list children)))

;Define the empty graph 
(define graph '())

(define (split-path path)
(regexp-split #rx"/" path))

(define (contains? lst element)
  (cond ((empty? lst) #f)
        ((equal? (car lst) element) #t)
        (else (contains? (cdr lst) element))))

(define (get-last lst)
(cond ((empty? lst) "")
  ((empty? (cdr lst)) (car lst))
  (else (get-last (cdr lst)))))

; Graph mutator. All modify operations use this procedure 
(define (modify-graph newGraph)
 (set! graph newGraph)
  graph)

(define (file-exists? name)
  (define (file-exists-helper? system)
    (cond ((empty? system) #f)
          ((equal? (caar system) name) #t)
          (else (file-exists-helper? (cdr system)))))

  (file-exists-helper? graph))

(define (merge-nodes node1 node2)
(gen-node (car node1) (append (cadr node1) (cadr node2))) 
  )


(define (add node)
  (modify-graph (cons node graph))

  )
  ;(cond ((file-exists? (car node)) (modify-graph (updateNode (merge-nodes node (get-file (car node))))))
        ;(else (modify-graph (cons node graph)))))

; Generic procedure which removes elements from list that match certain condition
(define (remove-element elements condition?)
  (cond ((empty? elements) elements)
        ((condition? (car elements)) (remove-element (cdr elements) condition?))
        (else (cons (car elements) (remove-element (cdr elements) condition? )))))

(define (delete node)
  (define (delete-helper system)
    (cond ((empty? system) '())
          (empty? (car system) '())
          ((equal? (caar system) (car node)) '())
          (else (cons (car system) (delete-helper (cdr system))))))

  (delete-helper graph))

 

(define (remove-all names)
(cond ((empty? names) "")
      (else (remove (car names)) (remove-all (cdr names)))))
 
(define (get-file name)
  (define (get-file-helper system)
    (cond ((empty? system) -1)
          ((equal? (caar system) name) (car system)) 
          (else (get-file-helper (cdr system)))))
  
  (get-file-helper graph))

(define (has-child? file child)
  (contains? (cadr file) child))

(define (get-file-path path)
  (define (helper pathList result)
    (cond ((empty? pathList) result)
          ((has-child? result (car pathList)) (helper (cdr pathList) (get-file (car pathList))))
          (else "No such file")))
  (helper (split-path path) (get-file "/")))

(define (genPath listPath)
    (cond ((empty? listPath) '())
          ((empty? (cdr listPath)) (cons (list (car listPath) '()) (genPath (cdr listPath))))
          (else (cons (list (car listPath) (list (cadr listPath))) (genPath (cdr listPath))))))

(define (updateNode updatedNode)

  (define (updateNode-helper system)
    (cond ((empty? system) '())
          ((equal? (caar system) (car updatedNode)) (cons updatedNode (updateNode-helper (cdr system))))
          (else (cons (car system) (updateNode-helper (cdr system))))))

  (updateNode-helper graph))
  


   

(define (mkdir path)
  (define (mkdir-helper listPath)
    (cond ((empty? listPath) "")
          ((empty? (cdr listPath)) (gen-node (car listPath) '()))
          (else (add (gen-node (car listPath) (list (cadr listPath)))) (mkdir-helper (cdr listPath)))))
    
(mkdir-helper (append '("/") (split-path path))))


(define (rmdir path)
(delete (get-last (split-path path))))



;(add (gen-node "/" '("1")))
;(add (gen-node "/" '("2")))
;(add (gen-node "/" '("2" "5")))
;(add (gen-node "dir" '("dir2")))
;(mkdir "a/b/c/d/f")
;(rmdir "a/b/c")
(add (gen-node "/" '("1")))
(add (gen-node "/" '("2")))



graph

