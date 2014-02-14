(load "./lab01.lsp")
(load "./lisp-unit.lisp")
(use-package :lisp-unit)

(define-test my-rotate
  (assert-equal (my-rotate '(a b c)) '(b c a))
)

(define-test my-rotate-n
  (assert-equal (my-rotate-n 3 '(a b c d)) '(d a b c))
)

(define-test first_sat
  (assert-equal (first_sat '(1 4 3 5) '(2 5 1 4) #'(lambda (x y) (> x y))) '(3 1))
)

(define-test my-remove
  (assert-equal (my-remove `b `(a b c d)) `(a c d))
  (assert-equal (my-remove `b `((a b) b (c b d e a) (b) (a) c)) `((a) (c d e a) NIL (a) c))
)

(define-test palindromep
  (assert-equal (palindromep `(b c c b)) T)
  (assert-equal (palindromep `(b c a c b)) T)
  (assert-equal (palindromep `(a b c)) NIL)
  (assert-equal (palindromep `(a (b a f) 1 (b a f) a)) T)
  (assert-equal (palindromep `(a (b a f) 1 (f a b) a)) NIL)
)

(run-tests)