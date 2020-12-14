(val applyNTimes 
   (cons 
      (lambda ($closure n) 
         (cons 
            (lambda ($closure f) 
               (cons 
                  (lambda ($closure x) 
                     (if (= (car (cdr (cdr $closure))) 0) 
                        x 
                        (((applyNTimes 
                                 (- (car (cdr (cdr $closure))) 1)) 
                              (car (cdr $closure))) 
                           ((car (cdr $closure)) x)))) 
                  (cons f (cons n '())))) 
            (cons n '()))) 
      '()))
(val twice (cons (lambda ($closure x) (* x 2)) '()))
(val applyTwice2Times (((applyNTimes 2) twice) 10))
(check-expect applyTwice2Times 40)
