;;;;
;       File: lab03.lsp
;     Author: Mervin Fansler
;      Class: CSCI 450
; Assignment: Lab 3: Searching Algorithms
;       Date: 3 March 2014
;;;;

; make-open-init
;
; Note: I feel like there should be a more idiomatic way of doing this

(defun make-open-init (xs) (list (list (list NIL xs))))

; extend-path 

(defun extend-path (dxs) (
  mapcar 
    #'(lambda (dx) (cons dx dxs)) ; append previous moves
	(remove-redundant ; cull cycles
	  (moves (get-state (car dxs))) ; get new moves
	  (cdr dxs)
    )
))

; search-bfs

(defun search-bfs (dxss) (
  cond
  ((null dxss) NIL) ; ran out of nodes
  ((goal-state (get-state (caar dxss))) (path (car dxss))) ; found solution
  (T (search-bfs (append (cdr dxss) (extend-path (car dxss))))) ; keep searching
))

; search-dfs-fd

(defun search-dfs-fd (dxss d) (
  cond
  ((or (< d 0) (null dxss) (null (car dxss))) NIL) ; ran out of nodes
  ((goal-state (get-state (caar dxss))) (path (car dxss))) ; found solution
  (T ( ; keep searching
    or 
    (search-dfs-fd (extend-path (car dxss)) (- d 1)) ; try going down
    (search-dfs-fd (cdr dxss) d) ; try going over
  ))
))

; search-id-iter
;
; helper function for iterative deepening search

(defun search-id-iter (dxss d) (
  or
  (search-dfs-fd dxss d) ; found an answer
  (search-id-iter dxss (+ d 1)) ; dig a little deeper
))

; search-id

(defun search-id (dxss) (search-id-iter dxss 0))

; sss

(defun sss (xs &key (type 'BFS) (depth 7)) (let ((xsss (make-open-init xs))) (
  cond
  ((eq type 'BFS) (search-bfs xsss))
  ((eq type 'DFS) (search-dfs-fd xsss depth))
  ((eq type 'ID) (search-id xsss))
))) 