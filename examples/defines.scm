(define add3
  (lambda (x) (+ 3 x)))
(define (add6 x) (+ 6 x))
(define four 4)

(add6
  (add3 four))
