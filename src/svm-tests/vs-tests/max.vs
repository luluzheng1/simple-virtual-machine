loadfunc r2 1 {
  loadfunc r0 1 {
    r2 := r0.0
    r2 := r2 > r1
    if r2 goto L1
    return r1
    goto L2
    def L1
    r0 := r0.0
    def L2
  }
  r0 := closure[r0 , 1]
  r0.0 := r1
  return r0
}
globals["max"] := r2
r0 := globals["max"]
r1 := 6
r0 := r0 call r1
r1 := 5
r0 := r0 call r1
print r0
