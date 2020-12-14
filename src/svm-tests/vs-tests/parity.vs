loadfunc r2 1 {
  loadfunc r2 1 {
    r2 := 0
    r2 := r1 = r2
    if r2 goto L1
    r2 := r0.0
    r3 := 1
    r3 := r1 - r3
    r3 := tailcall r2
    goto L2
    def L1
    r0 := false
    return r0
    def L2
  }
  r2 := closure[r2 , 1]
  loadfunc r3 1 {
    r2 := 0
    r2 := r1 = r2
    if r2 goto L3
    r2 := r0.0
    r3 := 1
    r3 := r1 - r3
    r3 := tailcall r2
    goto L4
    def L3
    r0 := true
    return r0
    def L4
  }
  r3 := closure[r3 , 1]
  r2.0 := r3
  r3.0 := r2
  mov r4 r2
  mov r5 r1
  r4 := r4 call r5
  if r4 goto L5
  r0 := "even"
  return r0
  goto L6
  def L5
  r0 := "odd"
  return r0
  def L6
}
globals["parity"] := r2
r0 := globals["parity"]
r1 := 0
r0 := r0 call r1
check r0 "(parity 0)"
r0 := "even"
expect r0 "'even"
r0 := globals["parity"]
r1 := 1
r0 := r0 call r1
check r0 "(parity 1)"
r0 := "odd"
expect r0 "'odd"
r0 := globals["parity"]
r1 := 30
r0 := r0 call r1
check r0 "(parity 30)"
r0 := "even"
expect r0 "'even"
r0 := globals["parity"]
r1 := 91
r0 := r0 call r1
check r0 "(parity 91)"
r0 := "odd"
expect r0 "'odd"
