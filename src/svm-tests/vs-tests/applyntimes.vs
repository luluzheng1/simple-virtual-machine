loadfunc r2 1 {
  loadfunc r0 1 {
    r1 := null? r2
    if r2 goto L1
    r2 := r0.0
    r1 := car r3
    r2 := r2 call r3
    if r2 goto L3
    r2 := globals["get_field"]
    r3 := r0.0
    r2 := r2 call r3
    r1 := cdr r3
    r3 := tailcall r2
    goto L4
    def L3
    r1 := car r2
    r2 := cdr r0
    return r0
    def L4
    goto L2
    def L1
    r0 := '()
    return r0
    def L2
  }
  r0 := closure[r0 , 1]
  r0.0 := r1
  return r0
}
globals["get_field"] := r2
loadfunc r2 1 {
  loadfunc r0 1 {
    loadfunc r0 1 {
      r2 := r0.1
      r3 := 0
      r2 := r2 = r3
      if r2 goto L5
      r2 := globals["applyNTimes"]
      r3 := r0.1
      r4 := 1
      r3 := r3 - r4
      r2 := r2 call r3
      r3 := r0.0
      r2 := r2 call r3
      r3 := r0.0
      mov r4 r1
      r3 := r3 call r4
      r3 := tailcall r2
      goto L6
      def L5
      return r1
      def L6
    }
    r0 := closure[r0 , 2]
    r0.0 := r1
    r0.1 := r1
    return r0
  }
  r0 := closure[r0 , 1]
  r0.0 := r1
  return r0
}
globals["applyNTimes"] := r2
loadfunc r2 1 {
  r2 := 2
  r0 := r1 * r2
  return r0
}
globals["twice"] := r2
loadfunc r2 1 {
  r0 := r1 * r1
  return r0
}
globals["square"] := r2
r0 := globals["applyNTimes"]
r1 := 2
r0 := r0 call r1
r1 := globals["twice"]
r0 := r0 call r1
r1 := 10
r0 := r0 call r1
globals["applyTwice2Times"] := r0
r0 := globals["applyNTimes"]
r1 := 10
r0 := r0 call r1
r1 := globals["twice"]
r0 := r0 call r1
r1 := 1
r0 := r0 call r1
globals["applyTwice10Times"] := r0
r0 := globals["applyNTimes"]
r1 := 2
r0 := r0 call r1
r1 := globals["square"]
r0 := r0 call r1
r1 := 10
r0 := r0 call r1
globals["applySquare2Times"] := r0
r0 := globals["applyTwice2Times"]
check r0 "applyTwice2Times"
r0 := 40
expect r0 "40"
r0 := globals["applyTwice10Times"]
check r0 "applyTwice10Times"
r0 := 1024
expect r0 "1024"
r0 := globals["applySquare2Times"]
check r0 "applySquare2Times"
r0 := 10000
expect r0 "10000"
