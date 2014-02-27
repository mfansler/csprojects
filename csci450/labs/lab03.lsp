;;;;
;       File: lab03.lsp
;     Author: Mervin Fansler
;      Class: CSCI 450
; Assignment: Lab 3: Searching Algorithms
;       Date: 26 February 2014
;;;;

; make-open-init

(defun make-open-init (xs) (list (list (list NIL xs))))

; extend-path 

(defun extend-path (dxs) (
  mapcar 
    #'(lambda (dx) (
	  append (list dx) dxs
	))
	(remove-redundant
	  (moves (get-state (car dxs)))
	  (cdr dxs)
    )
))

; search-bfs

(defun search-bfs (dxss) (
  cond
  ((null dxss) NIL)
  ((goal-state (get-state (caar dxss))) (path (car dxss)))
  (T (search-bfs (append (cdr dxss) (extend-path (car dxss)))))
))

; search-dfs-fd

(defun search-dfs-fd (dxss d) (
  cond
  ((or (< d 0) (null dxss) (null (car dxss))) NIL)
  ((goal-state (get-state (caar dxss))) (path (car dxss)))
  (T (
    or 
    (search-dfs-fd (extend-path (car dxss)) (- d 1))
    (search-dfs-fd (cdr dxss) d)
  ))
))

; search-id

(defun search-id-iter (dxss d) (
  or
  (search-dfs-fd dxss d)
  (search-id-iter dxss (+ d 1))
))

(defun search-id (dxss) (search-id-iter dxss 0))

; sss

(defun sss (xs &key (type 'BFS) (depth 7)) (let ((xsss (make-open-init xs))) (
  cond
  ((eq type 'BFS) (search-bfs xsss))
  ((eq type 'DFS) (search-dfs-fd xsss depth))
  ((eq type 'ID) (search-id xsss))
))) 