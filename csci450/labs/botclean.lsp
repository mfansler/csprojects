;; Enter your code here. Read input from STDIN. Print output to STDOUT

;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Utilities
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun state-to-simple-path (s) (zip (cons (car s) (butlast (cadr s))) (cadr s)))

(defun positions-to-moves (s) 
  (let ((x (- (caar s) (caadr s)))
        (y (- (cadar s) (cadadr s))))
       (append
         (make-list (abs x) :initial-element (if (> x 0) "LEFT" "RIGHT"))
         (make-list (abs y) :initial-element (if (> y 0) "UP" "DOWN"))
         (list "CLEAN"))
  )
)

(defun path-to-moves (p) (
  reduce #'(lambda (ms xs) (append ms (positions-to-moves xs))) p :initial-value ()))

(defun print-moves (ms) (dolist (m ms) (princ (concatenate 'string m (string #\linefeed)))))

(let* ((y (read)) (x (read))
      (s (read-state 0 (list (list x y) ()))))
  (princ (first (path-to-moves (state-to-simple-path s))))
  ;(print (path-to-moves (state-to-simple-path s)))
)
