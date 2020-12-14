;; literal
(let* ([$r0 3]) (check $r0 'three))
(let* ([$r0 3]) (expect $r0 'three))

;; name
(let* ([$r2 4] [$r3 $r2]) (check $r3 'four))
(let* ([$r3 4]) (expect $r3 'four))

;; vmop and vmop_lit
(let* ([$r0 2] [$r1 2] [$r0 (+ $r0 $r1)])
  (check $r0 'two-plus-two))
(let* ([$r0 5])
  (expect $r0 'five))

;; assign
(let* ([$r4 0] [$r5 3] [$r6 (set $r5 $r4)]) (check $r6 'zero!))
(let* ([$r7 0]) (expect $r7 'zero!))

;; seq
(let* ([$r1 (begin 1 2)]) (check $r1 'two))
(let* ([$r1 2]) (expect $r1 'two))

;; while
(let* ([$r0 0] 
       [$r1 1] 
       [$r2 2] 
       [$r3 (+ $r0 $r1)]
       [$r4 (while (let ([$r10 (< $r0 $r2)]) $r10) (set $r0 $r3))])
       (check $r0 'while-increment-five))
(let* ([$r0 3]) (expect $r0 'three))

;; if
(let* ([$r0 #t]
       [$r1 1]
       [$r2 0]
       [$r3 (if $r0 $r1 $r2)]) (check $r3 'one))