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
