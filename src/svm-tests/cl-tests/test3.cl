(val o 
   (cons 
      (lambda ($closure f g) 
         (cons 
            (lambda ($closure x) 
               ((car (cdr (cdr $closure))) ((car (cdr $closure)) x))) 
            (cons g (cons f '())))) 
      '()))
(val qsort 
   (cons 
      (lambda ($closure xs) 
         (if (null? xs) 
            '() 
            (let* ([pivot (car xs)]
                   [rest (cdr xs)]
                   [right? (cons 
                            (lambda ($closure n) 
                               (> n (car (cdr $closure)))) 
                            (cons pivot '()))]
                   [left? (o not right?)]) 
              (append 
                 (qsort (filter left? rest)) 
                 (cons pivot (qsort (filter right? rest))))))) 
      '()))
(val iota^ 
   (cons 
      (lambda ($closure n) (if (= n 0) '() (cons n (iota^ (- n 1))))) 
      '()))
(check-expect (qsort (cons 3 (cons 2 (cons 1 (cons 4 (cons 5 '())))))) 
   (reverse (iota^ 5)))
