(val a 3)
(val _ (print a ))
(define compose (f)(lambda (g) (lambda (x) (f (g x ) ))))
(define qsort (xs)(if (null xs ) 
'() 
(let ([pivot (car xs )]
      [rest (cdr xs )]
      [is_right (lambda (n) (greater (n pivot ) ))]
      [is_left (compose (not is_right ) )]) 
    (append ((qsort (filter (is_left rest )))) (cons (pivot (qsort (filter (is_right rest ) ) ) ) ) ) )))
