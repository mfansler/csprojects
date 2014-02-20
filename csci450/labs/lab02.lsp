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

(defun get-valid-directions (i) (
  multiple-value-bind (r c) (floor i 3) (
    append
      (if (> r 0) (list 'U) ())
      (if (< r 2) (list 'D) ())
      (if (> c 0) (list 'L) ())
      (if (< c 2) (list 'R) ())
  )
))

(defun swap-values (a b xs) (
  reduce #'(lambda (ys x) (
    cond
	((equal a x) (append ys (list b)))
	((equal b x) (append ys (list a)))
	(T (append ys (list x)))
  )) xs :initial-value ()
))

(defun swap-values-at (m n xs) (
  swap-values (nth m xs) (nth n xs) xs
))

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
	((eq d 'U) (list 'U (swap-values-at p (- p 3) xs)))
	((eq d 'D) (list 'D (swap-values-at p (+ p 3) xs)))
	((eq d 'L) (list 'L (swap-values-at p (- p 1) xs)))
	((eq d 'R) (list 'R (swap-values-at p (+ p 1) xs)))
  ))
  (get-valid-directions p)
)))