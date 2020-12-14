loadfunc r2 1 {
  r2 := false
  goto L1
  def L2
  r3 := "a"
  r4 := "b"
  r2 := r3 cons r4
  r3 := 1
  r1 := r1 - r3
  def L1
  r3 := 0
  r3 := r1 > r3
  if r3 goto L2
  return r2
}
globals["allocate"] := r2
r0 := globals["allocate"]
r1 := 1000
r0 := r0 call r1
check r0 "(allocate 1000)"
r0 := "a"
r1 := "b"
r0 := r0 cons r1
expect r0 "(cons 'a 'b)"
