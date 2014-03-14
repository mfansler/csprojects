; Solution set written by Stephanie Elzer
; CSCI 450, part 1 8-puzzle Assignment

(defun goal-state (state)
 (equal state '(1 2 3 8 e 4 7 6 5)))

(defun get-state (move)
 (first (rest move)))

(defun get-direction (move)
 (first move))

(defun same-state (move1 move2)
 (equal (get-state move1) (get-state move2)))

(defun path (pathlist)
 (rest (reverse (mapcar 'get-direction pathlist))))

(defun remove-redundant (list1 list2)
 (cond 
   ((null list1) nil)
   ((member (first list1) list2 :test #'same-state)
      (remove-redundant (rest list1) list2))
   (t (cons (first list1) (remove-redundant (rest list1) list2)))))

; Gets position of e in a board configuration
(defun getepos (state)
  (position 'e state))

; Swaps e and whatever value is at position n in state
(defun swap-e-with-n (state n)
  (let ((val (nth n state)))
    (substitute 'e 'x (substitute val 'e (substitute 'x val state)))))

; If moving down is legal from state, this function returns the resulting
; move in a list.  If moving down is not legal, an empty list is returned.
(defun move-down (state)
  (let ((epos (getepos state)))
       (if (> epos 5) () (list (list 'D (swap-e-with-n state (+ epos 3)))))))

; If moving up is legal from state, this function returns the resulting
; move in a list.  If moving up is not legal, an empty list is returned.
(defun move-up (state)
  (let ((epos (getepos state)))
       (if (< epos 3) () (list (list 'U (swap-e-with-n state (- epos 3)))))))

; If moving left is legal from state, this function returns the resulting
; move in a list.  If moving left is not legal, an empty list is returned.
(defun move-left (state)
  (let ((epos (getepos state)))
       (if (eq (mod epos 3) 0) 
           () 
           (list (list 'L (swap-e-with-n state (- epos 1)))))))

; If moving right is legal from state, this function returns the resulting
; move in a list.  If moving right is not legal, an empty list is returned.
(defun move-right (state)
  (let ((epos (getepos state)))
       (if (eq (mod epos 3) 2) 
           () 
           (list (list 'R (swap-e-with-n state (+ epos 1)))))))

 
; This is the main function (the only one that you need to call
; from your code).  It returns a list of the legal moves from the
; given state 
(defun moves (state)
  (append (move-down state)
          (move-up state)
          (move-left state)
          (move-right state)))
