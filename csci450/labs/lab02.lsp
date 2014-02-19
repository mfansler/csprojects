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

(defun path (dxs) (mapcar 'get-direction dxs))

; remove-redundant

(defun remove-redundant (dxs dys) NIL)

; moves 

(defun moves (xs) NIL)