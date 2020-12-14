(define mk-foo 
   (a) 
   (lambda (b) 
      (lambda (c) 
         (let* ([__tmpa (cons (cons 'a a) '())]
                [__tmpb (cons (cons 'b b) __tmpa)]
                [__tmpc (cons (cons 'c c) __tmpb)]) 
           __tmpc))))
(define get_field 
   (p?) 
   (lambda (xs) 
      (if (null? xs) 
         '() 
         (if (p? (car xs)) (cdr (car xs)) ((get_field p?) (cdr xs))))))
(define get_foo_a? (foo) (= (car foo) 'a))
(val get_foo_a (get_field get_foo_a?))
(define get_foo_b? (foo) (= (car foo) 'b))
(val get_foo_b (get_field get_foo_b?))
(define get_foo_c? (foo) (= (car foo) 'c))
(val get_foo_c (get_field get_foo_c?))
(val x (((mk-foo 3) 10) 13))
(val y (get_foo_a x))
(check-expect y 3)
