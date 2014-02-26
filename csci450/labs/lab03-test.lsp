; lab03-test.lsp

(load "./lab02.lsp")
(load "./lab03.lsp")
(load "./lisp-unit.lisp")
(use-package :lisp-unit)

(define-test make-open-init
  (assert-equal '(((NIL (2 8 3 1 6 4 7 e 5)))) (make-open-init '(2 8 3 1 6 4 7 e 5)))
)

(define-test extend-path
  (assert-equal 
    '(((U (2 8 3 1 e 4 7 6 5)) (NIL (2 8 3 1 6 4 7 e 5)))
	  ((L (2 8 3 1 6 4 e 7 5)) (NIL (2 8 3 1 6 4 7 e 5)))
	  ((R (2 8 3 1 6 4 7 e 5)) (NIL (2 8 3 1 6 4 7 e 5))))
	(extend-path (first (make-open-init '(2 8 3 1 6 4 7 e 5))))
  )
)

(define-test search-bfs
  (assert-equal '(U U L D R) (search-bfs (make-open-init '(2 8 3 1 6 4 7 e 5))))
)

(define-test search-dfs-fd
  (assert-equal '(U U L D R) (search-dfs-fd (make-open-init '(2 8 3 1 6 4 7 e 5)) 7))
)

(define-test search-id
  (assert-equal '(U U L D R) (search-id (make-open-init '(2 8 3 1 6 4 7 e 5))))
)

(define-test sss
  (assert-equal NIL (sss '(1 2 3 8 e 4 7 6 5)))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5)))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'BFS))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'DFS))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'DFS :depth 14))
  (assert-equal '(U U L D D R U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'DFS :depth 15))
  (assert-equal '(U U L D R) (sss '(2 8 3 1 6 4 7 e 5) :type 'ID))
)

(run-tests)