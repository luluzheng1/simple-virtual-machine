(define revapp 
   (xs) 
   (lambda (ys) 
      (if (null? xs) ys ((revapp (cdr xs)) (cons (car xs) ys)))))
(define reverse (xs) ((revapp xs) '()))
(define revapp 
   (xs) 
   (lambda (ys) 
      (if (null? xs) ys ((revapp (cdr xs)) (cons (car xs) ys)))))
(define reverse (xs) ((revapp xs) '()))
(define get_field 
   (p?) 
   (lambda (xs) 
      (if (null? xs) 
         '() 
         (if (p? (car xs)) (cdr (car xs)) ((get_field p?) (cdr xs))))))
(define list1 (x) (cons x '()))
(define curriedLt (x) (lambda (y) (< x y)))
(define curriedGt (x) (lambda (y) (> x y)))
(define mkInsertionSort 
   (op?) 
   (letrec  ([insert 
      (lambda (x) 
         (lambda (xs) 
            (if (null? xs) 
               (list1 x) 
               (if ((op? x) (car xs)) 
                  (cons x xs) 
                  (cons (car xs) ((insert x) (cdr xs)))))))] [sort 
      (lambda (xs) 
         (if (null? xs) '() ((insert (car xs)) (sort (cdr xs)))))]) 
      sort))
(val sortIncreasing (mkInsertionSort curriedLt))
(val sortDecreasing (mkInsertionSort curriedGt))
(define revapp.nr (xs ys)
  (if (null? xs)
    ys
      (revapp.nr (cdr xs) (cons (car xs) ys))))
;;
(define rev (xs) (revapp.nr xs '()))
;;
(define listN^ (n ns)
  (if (= n 0) ns (listN^ (- n 1) (cons n ns))))
(define listN (n) (rev (listN^ n '())))
(val x (listN 10))

(val my100List (listN 100))
(val my10000List (listN 10000))
(check-expect (sortIncreasing '(3 2 1)) '(1 2 3))
(check-expect (sortIncreasing '(6 9 1 7 4 3 8 5 2 10)) 
'(1 2 3 4 5 6 7 8 9 10))
(check-expect (sortDecreasing '(6 9 1 7 4 3 8 5 2 10)) 
   '(10 9 8 7 6 5 4 3 2 1))
(check-expect (sortIncreasing my100List) (reverse my100List))
;;
;;(define safe-sort (<)
;;  (letrec
;;      ([insert (lambda (prev' x xs)
;;                 (if (null? xs)
;;                     (revapp.nr prev' (cons x '()))
;;                     (if (< x (car xs))
;;                         (revapp.nr prev' (cons x xs))
;;                         (insert (cons (car xs) prev') x (cdr xs)))))]
;;       [sort (lambda (unsorted sorted)
;;               (if (null? unsorted)
;;                   sorted
;;                   (sort (cdr unsorted) (insert '() (car unsorted) sorted))))])
;;    (lambda (xs) (sort xs '()))))
;;
;;
