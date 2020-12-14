(define applyNTimes 
   (n) 
   (lambda (f) 
      (lambda (x) (if (= n 0) x (((applyNTimes (- n 1)) f) (f x))))))

(define twice (x) (* x 2))

(val applyTwice2Times (((applyNTimes 2) twice) 10))

(check-expect applyTwice2Times 40)
