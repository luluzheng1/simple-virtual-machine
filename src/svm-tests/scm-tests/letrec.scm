 (define mkInsertionSort
    (op?)
    (letrec  ([insert
       (lambda (x)
          (lambda (xs)
             (if x
                x
                (if x
                   (cons x xs)
                   x))))] [sort (lambda (x) (insert x))])
       sort))