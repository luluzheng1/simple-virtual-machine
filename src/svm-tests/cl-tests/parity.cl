(val parity 
   (cons 
      (lambda ($closure n) 
         (letrec  ([odd? 
            (cons 
               (lambda ($closure m) 
                  (if (= m 0) #f ((car (cdr $closure)) (- m 1)))) 
               (cons even? '()))] [even? 
            (cons 
               (lambda ($closure m) 
                  (if (= m 0) #t ((car (cdr $closure)) (- m 1)))) 
               (cons odd? '()))]) 
            (if (odd? n) 'odd 'even))) 
      '()))
(check-expect (parity 0) 'even)
(check-expect (parity 1) 'odd)
(check-expect (parity 30) 'even)
(check-expect (parity 91) 'odd)
