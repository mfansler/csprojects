;; Enter your code here. Read input from STDIN. Print output to STDOUT


(defun get-state (row state) (let ((line (read-line *standard-input* NIL))) (cond
  ((null line) state)
  (T 
    (get-state
      (+ row 1)
      (list (if (find #\m line) (list (position #\m line) row) (car state))
            (if (find #\p line) (list (position #\p line) row) (cadr state))
      )
    )
  )
)))

(defun state-to-moves (s) 
  (let ((x (- (caar s) (caadr s)))
        (y (- (cadar s) (cadadr s))))
       (append 
         (make-list (abs x) :initial-element (if (> x 0) "LEFT" "RIGHT"))
         (make-list (abs y) :initial-element (if (> y 0) "UP" "DOWN")))
  )
)

(defun print-moves (ms) (dolist (m ms) (princ (concatenate 'string m (string #\linefeed)))))

(let ((n (read))
      (s (get-state 0 '((-1 -1) (-1 -1)))))
  (print-moves (state-to-moves s))
)