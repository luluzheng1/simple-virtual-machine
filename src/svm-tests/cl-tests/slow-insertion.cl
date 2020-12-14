(val mkInsertionSort 
   (cons 
      (lambda ($closure op?) 
         (letrec  ([insert 
            (cons 
               (lambda ($closure x) 
                  (cons 
                     (lambda ($closure xs) 
                        (if (null? xs) 
                           (list1 (car (cdr (cdr (cdr $closure))))) 
                           (if (((car (cdr $closure)) 
                                    (car (cdr (cdr (cdr $closure))))) 
                                 (car xs)) 
                              (cons 
                                 (car (cdr (cdr (cdr $closure)))) 
                                 xs) 
                              (cons 
                                 (car xs) 
                                 (((car (cdr (cdr $closure))) 
                                       (car 
                                          (cdr (cdr (cdr $closure))))) 
                                    (cdr xs)))))) 
                     (cons 
                        (car (cdr $closure)) 
                        (cons 
                           (car (cdr (cdr $closure))) 
                           (cons x '()))))) 
               (cons op? (cons insert '())))] [sort 
            (cons 
               (lambda ($closure xs) 
                  (if (null? xs) 
                     '() 
                     (((car (cdr $closure)) (car xs)) 
                        ((car (cdr (cdr $closure))) (cdr xs))))) 
               (cons insert (cons sort '())))]) 
            sort)) 
      '()))
