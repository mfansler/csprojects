; lab02-test.lsp

(load "./lab02.lsp")
(load "./lisp-unit.lisp")
(use-package :lisp-unit)

(define-test goal-state
  (assert-equal NIL (goal-state '(2 8 3 1 6 4 7 e 5)))
  (assert-equal T (goal-state '(1 2 3 8 e 4 7 6 5)))
)

(define-test get-direction
  (assert-equal 'L (get-direction '(L (4 5 6 7 8 e 3 2 1))))
)

(define-test get-state
  (assert-equal '(4 5 6 7 8 e 3 2 1) (get-state '(L (4 5 6 7 8 e 3 2 1))))
)

(define-test same-state
  (assert-equal T (same-state '(L (4 5 6 7 8 e 3 2 1)) '(L (4 5 6 7 8 e 3 2 1))))
  (assert-equal T (same-state '(L (4 5 6 7 8 e 3 2 1)) '(U (4 5 6 7 8 e 3 2 1))))
  (assert-equal NIL (same-state '(L (6 5 4 7 8 e 3 2 1)) '(L (4 5 6 7 8 e 3 2 1))))
)

(define-test path
  (assert-equal
	'(U L D R)
	(path '((R (1 2 3 8 e 4 7 6 5))
			(D (1 2 3 e 8 4 7 6 5))
			(L (e 2 3 1 8 4 7 6 5))
			(U (2 e 3 1 8 4 7 6 5))
			(NIL (2 8 3 1 e 4 7 6 5)))
	)
  )
)

(define-test remove-redundant
  (assert-equal
	'(
	  (R (1 2 3 8 e 4 7 6 5))
	  (D (1 2 3 7 8 4 e 6 5))
	)
    (remove-redundant 
	  '(
	    (R (1 2 3 8 e 4 7 6 5))
		(U (e 2 3 1 8 4 7 6 5))
		(D (1 2 3 7 8 4 e 6 5))
	  )
	  '(
	    (D (1 2 3 e 8 4 7 6 5))
		(L (e 2 3 1 8 4 7 6 5))
		(U (2 e 3 1 8 4 7 6 5))
		(U (2 8 3 1 e 4 7 6 5))
	  )
	)
  )
)

(define-test get-valid-directions
  (assert-equal '(U D L R) (get-valid-directions 4))
  (assert-equal '(D R) (get-valid-directions 0))
  (assert-equal '(D L) (get-valid-directions 2))
  (assert-equal '(U D L) (get-valid-directions 5))
)

(define-test swap-at
  (assert-equal '(1 3 2 4 5) (swap-at 1 2 '(1 2 3 4 5)))
  (assert-equal '(1 4 3 2 5) (swap-at 1 3 '(1 2 3 4 5)))
  (assert-equal '(2 1 3 4 5) (swap-at 0 1 '(1 2 3 4 5)))
  (assert-equal '(5 2 3 4 1) (swap-at 0 4 '(1 2 3 4 5)))
  (assert-equal '(5 2 3 4 1) (swap-at 4 0 '(1 2 3 4 5)))
  (assert-equal '(1 2 3 4 5) (swap-at 3 3 '(1 2 3 4 5)))
  (assert-error 'type-error (swap-at 3 8 '(1 2 3 4 5)))
)

(define-test moves
  (assert-equal
	'((D (2 7 3 4 e 8 1 5 6))
	  (L (e 2 3 4 7 8 1 5 6))
      (R (2 3 e 4 7 8 1 5 6)))
	(moves '(2 e 3 4 7 8 1 5 6))
  )
)

(run-tests)