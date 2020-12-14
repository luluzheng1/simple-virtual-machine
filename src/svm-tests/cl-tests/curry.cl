
(val curry 
    (cons 
        (lambda 
            ($closure f) 
            (cons 
                (lambda 
                    ($closure x) 
                    (cons 
                        (lambda 
                            ($closure y) 
                            (                                (car 
                                    (cdr 
                                        (cdr $closure))) 
                                (car 
                                    (cdr $closure)) 
y))
                        (cons x 
                            (cons f '
                                ())))) 
                (cons f '
                    ()))) 
'
        ()))
