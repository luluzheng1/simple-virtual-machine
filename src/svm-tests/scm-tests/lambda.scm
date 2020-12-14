(define f (x)(add (x 1 ) ))
(define g (x)(add (x 2 ) ))
(define compose (f)(lambda (g) (lambda (x) (f (g x ) ))))
(define add3 (x)((compose (f g ) ) x ))
