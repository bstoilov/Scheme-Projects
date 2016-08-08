#lang racket
;Generates a node by a given name and children (nodeName ( child1 child2..))
(define (gen-node name children)
   (cons name (list children)))

;Generates a cons representing a text file (name . contents)
(define (gen-file name contents)
  (cons name contents))

;The current directory (pwd)
(define current "")

;Define the empty file system 
(define graph '())

;Return a list of the path splited by "/". "a/b/c" -> ("a" "b" "c")
(define (split-path path)
(regexp-split #rx"/" path))

;Checks if list 'l' contains element 'i'
(define (contains? l i)
  (if (empty? l) #f
      (or (equal? (first l) i) (contains? (rest l) i))))

; Graph mutator. All modify operations use this procedure 
(define (modify-graph newGraph)
 (set! graph newGraph))

;Checks if nodes is inside the graph, by its name (car node) = name
(define (node-exists? node)
  (not (equal? (get-node (car node)) -2)))

; Takse two nodes ('node1' and 'node2'), assuming they have the same names, merges thier children, WITHOUT duplicates
(define (merge-nodes node1 node2)
  (define (remove-duplicates l)
    (cond ((null? l)
           '())
          ((member (car l) (cdr l))
           (remove-duplicates (cdr l)))
          (else
           (cons (car l) (remove-duplicates (cdr l))))))
  (gen-node (car node1) (remove-duplicates (append (cadr node1) (cadr node2)))))

; Adds a node in the graph, if the node already exists, the new node is merged with the old one
(define (add node)
  (cond ((node-exists? node) (update-node (merge-nodes (get-node (car node)) node)))
        (else (modify-graph (cons node graph)))))


; Generic procedure which removes elements from list that match certain condition
(define (remove-element elements condition?)
  (cond ((empty? elements) elements)
        ((condition? (car elements)) (remove-element (cdr elements) condition?))
        (else (cons (car elements) (remove-element (cdr elements) condition? )))))

;Remove 'item' from list 'lst'
(define (remove-item item lst)
  (define (remove-item-cond? element)
    (equal? element item))
  (remove-element lst remove-item-cond?))

; Removes a node from the graph. (matches names (car node))
(define (remove-node name)
  (define (remove-node-cond? element)
    (equal? (car element) name))
  (modify-graph (remove-element graph remove-node-cond?)))

; Removes all children, of any node, with the specified 'name'
(define (remove-all-refs name)
  (define (remove-all-refs-helper system)
    (cond ((empty? system) "")
          (else (update-node (gen-node (caar system) (remove-item name (cadar system)))) (remove-all-refs-helper (cdr system)))))
  (remove-all-refs-helper graph))

; Retrieves a node by its name
(define (get-node name)
  (define (get-node-helper system)
    (cond ((empty? system) -2)
          ((equal? (caar system) name) (car system))
    (else (get-node-helper (cdr system)))))
(get-node-helper graph))

; Retrieves a file by its '/' separeted path
(define (get-file path)
  (define (has-child? file child)
    (contains? (cadr file) child))
  
  (define (helper pathList result)
    (cond ((empty? pathList) result)
          ((equal? result -2) "No such file!")
          ((has-child? result (car pathList)) (helper (cdr pathList) (get-node (car pathList))))
          (else -5)))
  
  (cond ((equal? path "/") (get-node "/"))
        (else  (helper (split-path path) (get-node "/")))))

; Finds the old node by name ( car old = car new ), and modifies the graph to contain the new node
(define (update-node updatedNode)
  (define (update-node-helper system)
    (cond ((empty? system) '())
          ((equal? (caar system) (car updatedNode)) (cons updatedNode (update-node-helper (cdr system))))
          (else (cons (car system) (update-node-helper (cdr system))))))

  (modify-graph (update-node-helper graph)))

;Rm removes the node by path and all of its references
(define (rm path)
  (remove-node path)
  (remove-all-refs path))
  
;Creates nodes to represent the dir path provided. Example "a/b" -> ( ("a" ("b")) ("b" '() ) )
(define (mkdir path)
  (define (mkdir-helper listPath)
    (cond ((empty? listPath) "")
          ((empty? (cdr listPath)) (add (gen-node (car listPath) '())))
          (else (add (gen-node (car listPath) (list (cadr listPath)))) (mkdir-helper (cdr listPath)))))
    
(mkdir-helper (append '("/") (split-path path))))


; Gets the current dir
(define (pwd) (string-append "/" current))

; Changes the current dir to the specified one ( if it exists )
(define (cd path)
  (define (gen-path)
    (cond ((equal? current "") (string-append current path))
          (else (string-append current "/" path))))
  
  (cond ((equal? (get-file (gen-path)) -5) "No such dir")
  (else (set! current (gen-path)))))

;Prints all children of the current dir
(define (ls)
  (cond ((equal? current "") (ls-target "/"))
        (else (ls-target current))))

;Prints all children of the specified by path dir
(define (ls-target path)
  (cond ((equal? (get-file path) -5) "No such file")
  (else (display (cadr (get-file path))) (newline))))

 ;Creates new text file, which will be child of the specified by 'path' dir 
(define (touch path name content)
  (cond ((equal? (get-file path) -5) "No such file!")
        (else (update-node (gen-node (car (get-file path)) (append (cadr (get-file path)) (list (gen-file name content))))))))

;Returns the last item of the list 'lst'
(define (get-last-elem lst)
  (cond ((empty? (cdr lst)) (car lst))
        (else (get-last-elem (cdr lst)))))

;Get the contents of the spcified by full path file.
(define (cat-file path)
  (define (cat-file-dir pathList)
    (cond ((empty? (cddr pathList)) (car pathList))
          (else (cat-file-dir (cdr pathList)))))
  (define (get-contents node name)
    (define (content-extract nodes)    
      (cond ((empty? nodes) "")
            ((and (cons? (car nodes)) (equal? (caar nodes) name)) (car nodes))
            (else (content-extract (cdr nodes)))))
    (content-extract (cadr node)))

  (cdr (get-contents (get-node (cat-file-dir (split-path path))) (get-last-elem (split-path path)))))

;Concatenates all text files specified by the paths list
(define (cat paths)
(cond ((empty? paths) "")
      (else (string-append (cat-file (car paths)) (cat (cdr paths))))))


;Example usage
(mkdir "dir1/dir2/dir3/dir4")
(mkdir "dir1/testDir")
(mkdir "dir123")
(touch "dir1" "text file" "RANDOM TEXT")
(touch "dir1/dir2/dir3" "AnotherTextFile" "MORE RANDOM TEXT")
(cat-file "dir1/dir2/dir3/AnotherTextFile")
(cat-file "dir1/text file")
(cat '("dir1/dir2/dir3/AnotherTextFile"  "dir1/text file" ))
(define node (gen-node "a" '("b" "c")))
(pwd)
(ls)
(cd "dir1")
(pwd)
(ls-target "dir1/dir2")