(define test (x) (lambda (y) (lambda (z) (+ x (+ z y)))))


(val _ (((test 3) 5) 6))
(check-expect _ 14)