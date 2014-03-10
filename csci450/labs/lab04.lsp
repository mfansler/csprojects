;;;;
;       File: lab04.lsp
;     Author: Mervin Fansler
;      Class: CSCI 450
; Assignment: Lab 4: Heuristic Searching Algorithms
;       Date: 9 March 2014
;;;;

(load "./lab02.lsp")
(load "./lab03.lsp")

; out-of-place

(defun out-of-place (xs) (
  count-if-not
    #'(lambda (xg) (OR 
      (eq (car xg) 'E) ; e
      (eq (car xg) (cadr xg))) ; matches goal
    )
    (mapcar 'list xs '(1 2 3 8 e 4 7 6 5)) ; pair elements with goal elements
))

; out-of-place-f

(defun out-of-place-f (dxs) (
  + (length dxs) -1
    (out-of-place (get-state (car dxs)))
))

; get-manhattan-distance
;
; params:
;   p1 - first position given by flat index of 3x3 board
;   p2 - second position given by flat index of 3x3 board
;
; return: manhattan distance between p1 and p2

(defun get-manhattan-distance (p1 p2) (
  multiple-value-bind (y1 x1) (floor p1 3) (
  multiple-value-bind (y2 x2) (floor p2 3) (
    + (abs (- y1 y2)) (abs (- x1 x2))
  )))
)

; manhattan

(defun manhattan (xs) (
  reduce #'(lambda (sum gp) (
    + sum (get-manhattan-distance (position (car gp) xs) (cadr gp))
  ))
  ; (tile value, position) pairs for goal state
  '((1 0) (2 1) (3 2) (4 5) (5 8) (6 7) (7 6) (8 3))
  :initial-value 0
))

; manhattan-f

(defun manhattan-f (dxs) (
  + (length dxs) -1
    (manhattan (get-state (car dxs)))
))

; better

(defun better (f) #'(lambda (p1 p2) (
  <= (funcall f p1) (funcall f p2)
)))

; search-id

(defun search-a* (dxss f) (let ((f-order (better f))) (
  cond
  ((null dxss) NIL) ; ran out of nodes
  ((goal-state (get-state (caar dxss))) (path (car dxss))) ; found solution
  (T ( ; keep searching
    search-a* (
      merge 'list 
	    (cdr dxss) ; remaining paths
	    (sort (extend-path (car dxss)) f-order) ; extend first, sort by heuristic
	    f-order ; merge by heuristic
	  )
	  f
	))
)))

; sss

(defun sss (xs &key (type 'BFS) (depth 7) (f 'out-of-place-f)) (let ((dxss (make-open-init xs))) (
  cond
  ((eq type 'BFS) (search-bfs dxss))
  ((eq type 'DFS) (search-dfs-fd dxss depth))
  ((eq type 'ID) (search-id dxss))
  ((eq type 'A*) (search-a* dxss f))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Answers for performance results
;
; 1. A* surpasses the others.
; 2. The manhattan-f (0.016 sec) heuristic outperforms out-of-place-f (0.047 sec).
; 3. By setting the depth parameter to 11, the DFS performance improves (0.062 sec)
;    but does not surpass either of the A* runs.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;