(define ref (x) (cons x '()))

(define deref (x) (car x))

(define floor (x) (if (< x 1) 0 (+ 1 (floor (- x 1)))))
(define math1 
   (q) 
   (lambda (r) (lambda (t) (floor (/ (* 10 (+ (* 3 q) r)) t)))))
(define math2 
   (q) 
   (lambda (k) 
      (lambda (r) 
         (lambda (l) 
            (lambda (t) 
               (let* ([sevenk (* 7 k)]
                      [rtimesl (* r l)]
                      [lhs (+ (* q sevenk) (+ 2 rtimesl))]
                      [rhs (* t l)]
                      [division (floor (/ lhs rhs))]) 
                 division))))))
(define calcPi 
   (n) 
   (let* ([new_n (ref n)]
          [q (ref 1)]
          [r (ref 0)]
          [t (ref 1)]
          [k (ref 1)]
          [z (ref 3)]
          [l (ref 3)]
          [nr (ref 0)]
          [nn (ref 0)]) 
     (while (> (deref new_n) 0) 
        (begin 
           (if (< 
                 (- (+ (* 4 (deref q)) (deref r)) (deref t)) 
                 (* (deref z) (deref t))) 
              (begin 
                 (set-car! new_n (- (deref new_n) 1)) 
                 (print (deref z)) 
                 (set-car! 
                    nr 
                    (* 10 (- (deref r) (* (deref z) (deref t))))) 
                 (set-car! 
                    z 
                    (- 
                       (((math1 (deref q)) (deref r)) (deref t)) 
                       (* 10 (deref z)))) 
                 (set-car! q (* (deref q) 10)) 
                 (set-car! r (deref nr))) 
              (begin 
                 (set-car! 
                    nr 
                    (* (+ (* 2 (deref q)) (deref r)) (deref l))) 
                 (set-car! 
                    nn 
                    (((((math2 (deref q)) (deref k)) (deref r)) 
                          (deref l)) 
                       (deref t))) 
                 (set-car! q (* (deref q) (deref k))) 
                 (set-car! t (* (deref t) (deref l))) 
                 (set-car! l (+ (deref l) 2)) 
                 (set-car! k (+ (deref k) 1)) 
                 (set-car! z (deref nn)) 
                 (set-car! r (deref nr))))))))


;; Edit below to make the test more computationally difficult
;; Will push ~1 variant once nested closures work.
(val _ (calcPi 10))
