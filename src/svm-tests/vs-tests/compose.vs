loadfunc r3 2 {
  mov r3 r2
  mov r4 r1
  loadfunc r0 1 {
    r2 := r0.1
    r3 := r0.0
    mov r4 r1
    r3 := r3 call r4
    r3 := tailcall r2
  }
  r0 := closure[r0 , 2]
  r0.0 := r3
  r0.1 := r4
  return r0
}
globals["o"] := r3

;; '(define o (f g) (lambda (x) (f (g x))))'