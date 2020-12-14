(define get_field 
   (p?) 
   (lambda (xs) 
      (if (null? xs) 
         '() 
         (if (p? (car xs)) (cdr (car xs)) ((get_field p?) (cdr xs))))))
(define fib (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))
(check-expect (fib 10) 55)
(check-expect (fib 30) 832040)
