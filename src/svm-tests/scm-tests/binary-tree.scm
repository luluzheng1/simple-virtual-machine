(val N 6) ;; enumerate depths N to N+K, takes about 1s
(val K 8)

(set &gamma 9)

;; Full description at
;; https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/binarytrees.html

;; This is a GC benchmark.  The idea is
;;
;;  1. Allocate a large tree, which grows the heap to its final size,
;;     and let that tree die immediately.
;;
;;  2. Allocate another "long-lived" tree, which will stay live for
;;     the duration of the benchmark.  This one will be copied at every
;;     GC, which is part of what we're measuring.
;;
;;  3. Repeat a whole bunch of times: allocate a smaller tree, count
;;     its nodes, and let it die immediately.



;; ported from Ruby code by Lulu Zheng and Norman Ramsey
;; from the Computer Language Benchmarks game 
;; see file binary-tree.rb


(define mk-tree 
   (left) 
   (lambda (right) 
      (let* ([__tmpleft (cons (cons 'left left) '())]
             [__tmpright (cons (cons 'right right) __tmpleft)]) 
        __tmpright)))

(define get_field 
   (p?) 
   (lambda (xs) 
      (if (null? xs) 
         '() 
         (if (p? (car xs)) (cdr (car xs)) ((get_field p?) (cdr xs))))))

(define get_tree_left? (tree) (= (car tree) 'left))
(val get_tree_left (get_field get_tree_left?))
(define get_tree_right? (tree) (= (car tree) 'right))
(val get_tree_right (get_field get_tree_right?))

(define item_check 
   (tree) 
   (if (null? (get_tree_left tree)) 
      1 
      (begin 
         (+ 
            1 
            (+ 
               (item_check (get_tree_left tree)) 
               (item_check (get_tree_right tree)))))))

(define bottom_up_tree
   (depth) 
   (if (<= depth 0) 
      ((mk-tree '()) '()) 
      (let* ([ndepth (- depth 1)]
             [l (bottom_up_tree ndepth)]) 
        ((mk-tree l) l))))

(define bottom-up-tree (depth)
  (if (> depth 0)
      (begin
        (set depth (- depth 1))
        (cons (bottom-up-tree depth) (bottom-up-tree depth)))
      (cons #f #f)))

(define item-check (tree)
  (if (car tree)
      (+ 1 (+ (item-check (car tree)) (item-check (cdr tree))))
      1))

(define max (x y) (if (> x y) x y))

(val depth 0)

(define power_helper 
   (x) 
   (lambda (n) 
      (lambda (res) 
         (if (= n 0) res (((power_helper x) (- n 1)) (* res x))))))

(define power (x) (lambda (n) (((power_helper x) n) 1)))

(val printed-digits '()) ;; in reverse order
(define print-digit (n)
  (set printed-digits (cons n printed-digits)))

(record result [count depth check])
  ;; number of trees, their depth, checksum

(define count-nodes (i n) ;; count nodes of a tree of depth n
   (let ([check 0]
         [start 1])
      (begin
         (while (<= start i) 
            (begin 
               (set check (+ check (item-check (bottom-up-tree n))))
               (set start (+ start 1))))
         check)))

(define run-iterations (n max_d min_d) ;; runs 2^n iterations at depth n
   (let* ([iterations ((power 2) (+ (- max_d n) min_d))]
          [check 0])
         (begin
            (set check (count-nodes iterations n))
            (print-digit check))))

(val stretch_tree '())
(val long_lived_tree '())

(define binary_trees 
   (min_depth) 
   (lambda (max_depth) 
     (let* ([depth min_depth]
            [max_depth (max max_depth (+ 2 min_depth))]
            [stretch_depth (+ 1 max_depth)])
       (begin
         (set stretch_tree (bottom_up_tree stretch_depth))
         (set long_lived_tree (bottom_up_tree max_depth))
         (while (<= depth max_depth) 
            (begin 
               (run-iterations depth max_depth min_depth)
               (set depth (+ depth 2))))))))

(define run-trees (k n)
  (begin
    ((binary_trees k) n)
    (let* ([ds (reverse printed-digits)]
           [_ (set printed-digits '())])
      ds)))

;(check-expect (run-trees 4 6) '(1024 1792 1984 2032))
(check-expect (run-trees N (+ N K)) '(2080768 2093056 2096128 2096896 2097088))
(check-expect (item_check stretch_tree) 65535)
(check-expect (item_check long_lived_tree) 32767)
;(check-expect (run-trees 6 12) '(520192 523264 524032 524224))