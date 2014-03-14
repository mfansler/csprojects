(load "solnA1.lisp")

;;; Program specifications by Gary Cottrell
;;; Code written by Stephanie Elzer 10/2/03

;;; Main function: SSS
;;; Takes one required parameter,
;;; the initial state.
;;; Two optional parameters:
;;;    -type: can be BFS (default) or DFS.
;;;    -depth: a depth bound for DFS (default: 7)
;;; SSS checks whether we are at the goal.
;;; If not, it constructs the initial open list
;;; and passes control to an au illiary function that
;;; actually does the searching.
;;; The open list will be a list of lists, of the form:
;;; (<path 1> <path 2> .... <path n>)
;;; where <path i> is of the form:
;;; ((<move_k> <state_k>) (<move_k-1> <state k-1>) ...(<move_1> <state 1>))

(defun SSS (state &key (type 'BFS) (depth 7))
   (cond
   ((goal-state state) nil)
   ((equal type 'BFS) (search-BFS (make-open-init state)))
   ((equal type 'DFS) (search-DFS (make-open-init state) depth))
   ((equal type 'ID) (search-ID (make-open-init state)))))

(defun search-BFS (open)
   ;; check for failure
   (cond 
   ((null open) nil)
   ;; check for goal state
   ((goal-state (get-state (caar open))) (path (car open)))
   ;; call search-BFS with new open list
   (t (search-BFS (append (cdr open)
                          (extend-path (car open))))))) 

(defun search-DFS (open depth)
   ;; check for failure
   (cond
   ((null open) nil)
   ;; check for goal state
   ((goal-state (get-state (caar open))) (path (car open)))
   ;; check if current path exceeds depth 
   ((> (length (car open)) depth) (search-DFS (rest open) depth))
   (t (search-DFS (append (extend-path (car open))
                          (cdr open)) depth))))

(defun search-ID (open &optional (curr-depth 0))
   (let ((current-dfs (search-DFS open curr-depth)))
   (cond
   ((null current-dfs) (search-ID open (+ 1 curr-depth)))
   (t current-dfs))))

(defun extend-path (path)
  (let* ((tempmoves (moves (get-state (first path))))
         (newmoves (remove-redundant tempmoves path)))
  (extend-helper newmoves path)))

(defun extend-helper (movelst path)
   (cond
   ((null movelst) nil)
   (t (cons (cons (car movelst) path) (extend-helper (cdr movelst) path)))))

(defun make-open-init (state)
   (list (list (list nil state))))



