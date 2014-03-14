; lab04-test.lsp

(load "./lab04.lsp")

(load "./lisp-unit.lisp")
(use-package :lisp-unit)

(define-test out-of-place
  (assert-equal 4 (out-of-place '(2 8 3 1 6 4 7 e 5)))
  (assert-equal 0 (out-of-place '(1 2 3 8 e 4 7 6 5)))
)

(define-test out-of-place-f
  (assert-equal 4
	(out-of-place-f '((D (1 2 3 e 8 4 7 6 5))
					  (R (e 2 3 1 8 4 7 6 5))
					  (U (1 2 3 4 8 7 3 6 5))
					  (NIL (1 2 3 e 8 7 4 6 5)))
	)
  )
  (assert-equal 4
	(out-of-place-f '((R (e 2 3 1 8 4 7 6 5))
					  (U (1 2 3 4 8 7 3 6 5))
					  (NIL (1 2 3 e 8 7 4 6 5)))
	)
  )
)

(define-test get-manhattan-distance
  (assert-equal 0 (get-manhattan-distance 0 0))
  (assert-equal 0 (get-manhattan-distance 3 3))
  (assert-equal 0 (get-manhattan-distance 7 7))
  (assert-equal 1 (get-manhattan-distance 0 1))
  (assert-equal 1 (get-manhattan-distance 0 3))
  (assert-equal 2 (get-manhattan-distance 0 4))
  (assert-equal 2 (get-manhattan-distance 4 8))
  (assert-equal 4 (get-manhattan-distance 2 6))  
)

(define-test manhattan
  (assert-equal 11 (manhattan '(2 1 4 8 6 5 3 e 7)))
  (assert-equal 1 (manhattan '(1 2 3 e 8 4 7 6 5)))
)

(define-test manhattan-f
  (assert-equal 4
	(manhattan-f '((D (1 2 3 e 8 4 7 6 5))
				   (R (e 2 3 1 8 4 7 6 5))
				   (U (1 2 3 4 8 7 3 6 5))
				   (NIL (1 2 3 e 8 7 4 6 5)))
	)
  )
)

(define-test better 
  (assert-equal T (funcall (better #'out-of-place-f) 
    `((U (2 8 3 1 e 4 7 6 5)) (NIL (2 8 3 1 6 4 7 e 5)))
    `((U (1 2 3 8 6 e 7 5 4)) (NIL (1 2 3 8 6 4 7 5 e)))
  ))
  (assert-equal T (funcall (better #'out-of-place-f)
    `((U (1 2 3 8 6 e 7 5 4)) (NIL (1 2 3 8 6 4 7 5 e)))
    `((U (2 8 3 1 e 4 7 6 5)) (NIL (2 8 3 1 6 4 7 e 5)))
  ))
  (assert-equal NIL (funcall (better #'out-of-place-f)
    `((L (2 1 4 8 e 7 3 6 5)) (NIL (1 2 4 8 7 e 3 6 5)))
    `((U (1 2 3 8 6 e 7 5 4)) (NIL (1 2 3 8 6 4 7 5 e)))
  ))
)

(define-test search-a*
  (assert-equal '(U R R D D L U L U R D)
    (search-a* (make-open-init '(2 8 1 e 6 3 7 5 4)) 'out-of-place-f)
  )
  (assert-equal '(U R R D D L U L U R D)
    (search-a* (make-open-init '(2 8 1 e 6 3 7 5 4)) 'manhattan-f)
  )
  (assert-equal NIL
    (search-a* () 'manhattan-f)
  )
)

(define-test sss
  (assert-equal NIL (sss '(1 2 3 8 e 4 7 6 5)))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5)))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'BFS))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'DFS))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'DFS :depth 14))
  (assert-equal '(U U L D D R U L D R U L D R U) (sss '(2 8 3 1 6 4 7 e 5) :type 'DFS :depth 15))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'ID))
  (assert-equal '(U R R D D L U L U R D) (sss '(2 8 1 e 6 3 7 5 4) :type 'A*))
  (assert-equal '(U R R D D L U L U R D) (sss '(2 8 1 e 6 3 7 5 4) :type 'A* :f 'manhattan-f))
)

(run-tests)