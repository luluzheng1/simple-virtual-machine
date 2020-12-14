;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;



;; (mirror xs) returns a list in which every list in xs is 
;;             recursively mirrored, and the resulting lists
;;             are in reverse order.          

;; laws:
;;   (mirror '()) = '()
;;   (mirror (cons z zs)) = (append (mirror zs) (list1 (mirror z))), 
;;                          where z is a nonempty list
;;   (mirror (cons z zs)) = (append (mirror zs) (list1 z)),
;;                          where z is not a nonempty list
   
(define mirror (xs)
    (if (null? xs)
        xs
        (if (pair? (car xs))
            (append (mirror (cdr xs)) (list1 (mirror (car xs))))
            (append (mirror (cdr xs)) (list1 (car xs))))))

;; tests:
(check-expect (mirror '()) '())
(check-expect (mirror '(1 2 3 4 5)) '(5 4 3 2 1))
(check-expect (mirror '((a (b 5)) (c d) e)) '(e (d c) ((5 b) a)))



;; (flatten xs) constricts a list having the same atoms as xs in the
;;              same order and erases internal parentheses; xs is a 
;;              list of S-expressions.

;; laws:
;;   (flatten '()) == '()
;;   (flatten (cons z zs)) == (append z (flatten zs))), 
;;                            where z is a list
;;   (flatten (cons z zs)) == (cons z (flatten zs))),
;;                            where z is not a list
(define flatten (xs)
    (if (null? xs)
        xs
        (if (pair?(car xs))
            (append (flatten (car xs)) (flatten (cdr xs)))
            (cons (car xs) (flatten (cdr xs))))))

;; tests:
(check-expect (flatten '()) '())
(check-expect (flatten '((I Ching) (U Thant) (E Coli))) 
               '(I Ching U Thant E Coli))
(check-expect (flatten '((a b) (c d) e)) 
               '(a b c d e))



;; Helper Function(s) for contig-sublist?

;; (front-sublist? xs ys) determines whether ys begins with the subsequence xs;
;;                        returns #t if and only if xs is the front of ys
;;                        such that (append xs zs) == ys; zs is a list.                        

;; laws:
;;   (front-sublist? '() bs) == #t
;;   (front-sublist? as '()) == #f
;;   (front-sublist? (cons a as) (cons b bs)) == #t, where a = b
;;   (front-sublist? (cons a as) (cons b bs)) == #f, where a != b

(define front-sublist? (xs ys)
    (if (null? xs)
    #t 
        (if (null? ys)
            #f
            (if (equal? (car xs) (car ys))
                (front-sublist? (cdr xs) (cdr ys))
                #f))))                      

;; (contig-sublist? xs ys) determines whether the list xs is a contiguous
;;                         subsequence of the list ys; returns #t if and only
;;                         if there are two other lists, front and back, such
;;                         that ys is equal to (append (append front xs) back)

;; laws:
;;   (contig-sublist? '() ys) == #t
;;   (contig-sublist? (cons a as) (cons b bs)) == 
;;       (front-sublist? (cons a as) (cons b bs)), where a = b                        
;;   (contig-sublist? (cons a as) (cons b bs)) == 
;;       (contig-sublist? (cons a as) (cons b bs)), where a != b

(define contig-sublist? (xs ys)
    (if (null? xs)
        #t 
        (if (equal? (car xs) (car ys))
            (front-sublist? xs ys)
            (contig-sublist? xs (cdr ys)))))
        
;; tests
(check-assert (not(contig-sublist? '(a b c) '(x a y b z c))))
(check-assert (contig-sublist? '(a y b) '(x a y b z c)))
(check-assert (contig-sublist? '(x) '(x a y b z c)))
(check-assert (contig-sublist? '() '(x a y b z c)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (take n xs) takes a natural number n and a list of values xs
;;             and returns the longest prefix of xs that contains
;;             at most n elements

;; laws:
;;   (or (take 0 xs) (take n '())) == '()
;;   (take n (cons a as)) == (append (list1 a) (take (- n 1) as)) 

(define take (n xs)
    (if (or (= n 0) (null? xs))
        '()
        (append (list1 (car xs)) (take (- n 1) (cdr xs)))))

;; tests
(check-expect (take 1 '(1 2 3 4 5)) '(1)) 
(check-expect (take 1 '()) '()) 
(check-expect (take 6 '(1 2 3 4 5)) '(1 2 3 4 5)) 



;; (drop n xs) takes a natural number n and a list of values xs
;;             and a list of values xs and returns a new list
;;             with the first n elements of xs removed. 

;; laws:
;;   (or (drop 0 xs) (drop n '())) == xs
;;   (drop n (cons a as)) == (drop (- n 1) as)

(define drop (n xs)
    (if (or (= n 0) (null? xs))
        xs
        (drop (- n 1) (cdr xs))))

;; tests
(check-expect (drop 1 '(1 2 3 4 5)) '(2 3 4 5))
(check-expect (drop 0 '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (drop 3 '(1 2 3 4 5)) '(4 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (zip xs ys) produces a list of pairs that associate the
;;             corresponding values in xs and ys. 

;; laws:
;;   (zip '() '()) == '()
;;   (zip (cons x xs) (con y ys)) == (cons (list2 x y) (zip xs ys))

(define zip (xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (cons (list2 (car xs) (car ys)) 
            (zip (cdr xs) (cdr ys)))))

;; tests
(check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
(check-expect (zip '() '()) '())
(check-expect (zip '(c d e) '(c d e)) '((c c) (d d) (e e)))



;; Helper Functions for Unzip

;; (list-key lp) takes a list of pairs and returns a list of all the keys
;;               in the pairs

;; laws:
;;  (list-key '()) == '()
;;  (list-key (cons a as)) == (cons (car a) (list-key as))

(define list-key (lp)
    (if (null? lp)
        '()
        (cons (caar lp) (list-key (cdr lp)))))

;; (list-attribute lp) takes a list of pairs and returns a list of all the
;;                     attributes in the pairs

;; laws:
;;  (list-key '()) == '()
;;  (list-key (cons a as)) == (append (cdr a) (list-atrribute as))

(define list-attribute (lp)
    (if (null? lp)
        '()
        (append (cdar lp) (list-attribute (cdr lp)))))



;; (unzip ps) takes a list of pairs and produces two lists containing
;;            the separate elements. 

;; laws:
;;   (unzip '()) == '()
;;   (unzip ps) == (cons (list-key ps) (list-attribute ps))


(define unzip (ps)
    (if (null? ps)
        '()
        (list2 (list-key ps) (list-attribute ps)))) 

;; tests
(check-expect (unzip '()) '())
(check-expect (unzip '((H W) (e o) (l r) (l l) (o d))) '((H e l l o) (W o r l d)))
(check-expect (unzip '((I Magnin) (U Thant) (E Coli))) '((I U E) (Magnin Thant Coli)))
