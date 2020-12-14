r0 := function 1 {
  r2 := function 1 {
    r2 := r0.0
    r3 := r0.1
    r0 := function 1 {
      r2 := null? r1
      if r2 then goto L1
      r2 := r0.0
      r3 := r0.2
      r2 := call r2 (r3)
      r3 := car r1
      r2 := call r2 (r3)
      if r2 then goto L2
      r2 := car r1
      r3 := r0.1
      r4 := r0.2
      r3 := call r3 (r4)
      r4 := cdr r1
      r3 := call r3 (r4)
      r0 := r2 cons r3
      return r0
      L2:
      r2 := r0.2
      r0 := r2 cons r1
      return r0
      L1:
      r2 := ["list1"]
      r3 := r0.2
      tailcall r2 (r3)
    }
    r0 := closure[r0, 3]
    r0.0 := r2
    r0.1 := r3
    r0.2 := r1
  return r0
  }
  r2 := closure[r2, 2]
  r3 := function 1 {
    r2 := null? r1
    if r2 then goto L3
    r2 := r0.0
    r3 := car r1
    r2 := call r2 (r3)
    r3 := r0.1
    r4 := cdr r1
    r3 := call r3 (r4)
    tailcall r2 (r3)
    L3:
    r0 := '()
    return r0
  }
  r3 := closure[r3, 2]
  r2.0 := r1
  r2.1 := r2
  r3.0 := r2
  r3.1 := r3
  return r3
}
["mkInsertionSort"] := r0
