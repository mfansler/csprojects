; Mervin Fansler
; CSCI 450
; Lab 2: More Intro
; 19 February 2014

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

; moves 

(defun get-valid-directions (i) (
  multiple-value-bind (r c) (floor i 3) (
    remove NIL (list
      (if (> r 0) 'U NIL)
      (if (< r 2) 'D NIL)
      (if (> c 0) 'L NIL)
      (if (< c 2) 'R NIL)
    )
  )
))

(defun moves (xs) (
  get-valid-directions (position 'e xs)
))