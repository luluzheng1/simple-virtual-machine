(val o 
   (cons 
      (lambda ($closure $closure f g) 
         (cons 
            (lambda ($closure x) 
               ((car (cdr $closure)) ((car $closure) x))) 
            (cons g (cons f '())))) 
      '()))
(val qsort 
   (cons 
      (lambda ($closure $closure xs) 
         (if (null? xs) 
            '() 
            (let* ([pivot (car xs)]
                   [rest (cdr xs)]
                   [right? (cons 
                            (lambda ($closure n) (> n (car $closure))) 
                            (cons pivot '()))]
                   [left? (o not right?)]) 
              (append 
                 (qsort (filter left? rest)) 
                 (cons pivot (qsort (filter right? rest))))))) 
      '()))
(val iota^ 
   (cons 
      (lambda ($closure $closure n) 
         (if (= n 0) '() (cons n (iota^ (- n 1))))) 
      '()))
(check-expect (qsort (cons 3 (cons 2 (cons 1 (cons 4 (cons 5 '())))))) 
   (reverse (iota^ 5)))
