(val o 
   (cons 
      (lambda ($closure f g) 
         (cons 
            (lambda ($closure x) 
               ((car (cdr (cdr $closure))) ((car (cdr $closure)) x))) 
            (cons g (cons f '())))) 
      '()))
