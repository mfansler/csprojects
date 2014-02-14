; my-rotate

(defun my-rotate (xs) (
  append (cdr xs) (list (car xs))
))

; nest

(defun nest (f x n) (
  cond
  ((> n 0) (funcall f (nest f x (- n 1))))
  (T x)
))

; my-rotate-n

(defun my-rotate-n (n xs) (nest 'my-rotate xs n))

; first_sat

(defun first_sat (xs ys f) (
  if (or (null xs) (null xs)) () (
    let ((x (car xs)) (y (car ys))) (
      if (funcall f x y)
        (list x y)
	    (first_sat (cdr xs) (cdr ys) f)
    )
  )
))

; my-remove

(defun my-remove (a xs) (
  cond
  ((atom xs) (if (eq a xs) () xs))
  ((eq a (car xs)) (my-remove a (cdr xs)))
  (T (cons (my-remove a (car xs)) (my-remove a (cdr xs))))
))

; palindromep

(defun palindromep (xs) (
  cond
  ((null xs) T)
  ((equal (car xs) (car (last xs))) (palindromep (cdr (butlast xs))))
  (T NIL)
))
