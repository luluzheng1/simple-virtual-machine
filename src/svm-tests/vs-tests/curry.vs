loadfunc r2 1 {
  loadfunc r0 1 {
    loadfunc r0 1 {
      mov r2 r1
      r3 := r0.0
      mov r4 r1
      r4 := tailcall r2
    }
    r0 := closure[r0 , 1]
    r0.0 := r1
    return r0
  }
  r0 := closure[r0 , 1]
  r0.0 := r1
  return r0
}
globals["curry"] := r2
