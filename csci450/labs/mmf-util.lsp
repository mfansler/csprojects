; utilities.lsp
; Description: Miscellaneous helper functions accumulated as I write code for class.
; Author: Merv Fansler


; nest
; params:
;   f - function to nest
;   x - initial value to input into function
;   n - number of times to nest function
; example:
;   (nest 'f x 2) -> (f (f (x)))		  

(defun nest (f x n) (
  cond
  ((> n 0) (funcall f (nest f x (- n 1))))
  (T x)
))

; swap-all
; params:
;   a - value to replace with b
;   b - value to replace with a
;   xs - input list
; example:
;   (swap-all 0 1 '(0 0 1 1 0)) -> (1 1 0 0 1)

(defun swap-all (a b xs) (
  reduce #'(lambda (ys x) (
    cond
	((equal a x) (append ys (list b)))
	((equal b x) (append ys (list a)))
	(T (append ys (list x)))
  )) xs :initial-value ()
))

; swap-all-at 
; param:
;   m - position from which to select the first value
;   n - position from which to select the second value
;   xs - input list

(defun swap-all-at (m n xs) (
  swap-values (nth m xs) (nth n xs) xs
))

; swap-at
; param:
;   m - 1st swapping position [0-based index]
;   n - 2nd swapping position
;   xs - input list

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