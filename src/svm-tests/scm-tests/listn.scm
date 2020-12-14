(define get_field 
   (p?) 
   (lambda (xs) 
      (if (null? xs) 
         '() 
         (if (p? (car xs)) (cdr (car xs)) ((get_field p?) (cdr xs))))))
(define listN (n) (if (= n 0) '() (cons n (listN (- n 1)))))
(val myList (listN 10))
