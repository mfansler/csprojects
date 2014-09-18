;; Enter your code here. Read input from STDIN. Print output to STDOUT

(defun range (m n &optional (k 1)) (
  cond
    ((> 0 (* (- n m) k)) ())
	(T (cons m (range (+ m k) n k)))
))

; explode
(defun explode (s) (map 'list #'(lambda (c) c) s))

; zip
(defun zip (xs ys) (mapcar #'list xs ys))

; zip-with-positions
(defun zip-with-positions (xs) (
  zip xs (range 0 (- (length xs) 1))
))

; zip-all
(defun zip-all-right (xs y) (zip xs (make-list (length xs) :initial-element y)))
(defun zip-all-left (xs y) (zip (make-list (length xs) :initial-element y) xs))

; position-all
(defun position-all (hs needle) (
  mapcar #'cadr (
    remove-if-not #'(lambda (e) (equal needle (car e)))
	  (zip-with-positions hs))
))

(defun read-state (row state) (let ((line (read-line *standard-input* NIL))) (cond
  ((null line) state)
  (T 
    (read-state
      (+ row 1)
      (list (car state)
            (if (find #\d line) 
                (append 
                 (cadr state)
                 (zip-all-right (position-all (explode line) #\d) row))
                (cadr state))
      )
    )
  )
)))

(defun positions-to-moves (p1 p2) 
  (let ((x (- (car p1) (car p2)))
        (y (- (cadr p1) (cadr p2))))
       (append
         (make-list (abs x) :initial-element (if (> x 0) "LEFT" "RIGHT"))
         (make-list (abs y) :initial-element (if (> y 0) "UP" "DOWN"))
         (list "CLEAN"))
  )
)

(defun next_move (s) (cond
  ((find (car s) (cadr s) :test #'equal) (princ "CLEAN"))
  (T (princ (first (positions-to-moves (car s) (caadr s)))))
))

(defun get-distance (p1 p2) (+ (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2)))))

(defun distance-from-p (p) #'(lambda (p1 p2) (< (get-distance p p1) (get-distance p p2))))

(let* ((y (read)) (x (read)) (s (read-state 0 (list (list x y) ())))) 
  (next_move (list (car s) (sort (cadr s) (distance-from-p (car s)))))
)