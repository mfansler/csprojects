;;;;
;       File: lab03.lsp
;     Author: Mervin Fansler
;      Class: CSCI 450
; Assignment: Lab 3: Searching Algorithms
;       Date: 3 March 2014
;;;;

;;;;;;;;;;; begin concat lab02.lsp

;;;;
;       File: lab02.lsp
;     Author: Mervin Fansler
;      Class: CSCI 450
; Assignment: Lab 2: More Intro
;       Date: 20 February 2014
;;;;

; goal-state

(defun goal-state (xs) (equal xs '(1 2 3 8 e 4 7 6 5)))

; get-direction 

(defun get-direction (dx) (car dx))

; get-state

(defun get-state (dx) (cadr dx))

; same-state

(defun same-state (dx dy) (equal (get-state dx) (get-state dy)))

; path

(defun path (dxs) (reverse (remove NIL (mapcar 'get-direction dxs))))

; remove-redundant

(defun remove-redundant (dxs dys) (
  remove-if 
    #'(lambda (dx) (
      some #'(lambda (dy) (same-state dx dy)) dys
    ))
	dxs
  )
)

; get-valid-directions
; 
;  param: i - board index
; return: list of valid directions that the empty tile could move in

(defun get-valid-directions (i) (
  multiple-value-bind (r c) (floor i 3) (
    append
      (if (> r 0) (list 'U) ())
      (if (< r 2) (list 'D) ())
      (if (> c 0) (list 'L) ())
      (if (< c 2) (list 'R) ())
  )
))

; swap-at
; 
; params:
;   m - 1st swapping index
;   n - 2nd swapping index
;   xs - input list
; return: list with values at specified indices swapped

(defun swap-at (m n xs) (
  cond
  ((> m n) (swap-at n m xs))
  ((< m n) (let ((s (list-length xs))) (
    append
      (butlast xs (- s m))
	  (list (nth n xs))
	  (nthcdr (+ m 1) (butlast xs (- s n)))
	  (list (nth m xs))
	  (nthcdr (+ n 1) xs)
  )))
  (T xs)
))

; moves 

(defun moves (xs) (let ((p (position 'e xs))) (
  mapcar #'(lambda (d) (
    cond
	((eq d 'U) (list 'U (swap-at p (- p 3) xs)))
	((eq d 'D) (list 'D (swap-at p (+ p 3) xs)))
	((eq d 'L) (list 'L (swap-at p (- p 1) xs)))
	((eq d 'R) (list 'R (swap-at p (+ p 1) xs)))
  ))
  (get-valid-directions p)
)))

;;;;;;;;;;;;;;;;;; end concat lab02.lsp

;;;;;;;;;;;;;;;;;; begin lab03.lsp body

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