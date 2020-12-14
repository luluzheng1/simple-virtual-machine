r0 := 1
globals["N"] := r0
r0 := 0
globals["count"] := r0
loadfunc r2 1 {
  r2 := 0
  r2 := r1 = r2
  if r2 goto L1
  r2 := 0
  r3 := globals["mklist"]
  r4 := 1
  r4 := r1 - r4
  r3 := r3 call r4
  r0 := r2 cons r3
  return r0
  goto L2
  def L1
  r0 := '()
  return r0
  def L2
}
globals["mklist"] := r2
r0 := globals["mklist"]
r1 := globals["N"]
r0 := r0 call r1
globals["L"] := r0
loadfunc r4 3 {
  r4 := 0
  r4 := r2 = r4
  if r4 goto L3
  r4 := car r1
  r5 := globals["update"]
  r6 := cdr r1
  r7 := 1
  r7 := r2 - r7
  mov r8 r3
  r5 := r5 call r8
  r0 := r4 cons r5
  return r0
  goto L4
  def L3
  r4 := cdr r1
  r0 := r3 cons r4
  return r0
  def L4
}
globals["update"] := r4
loadfunc r3 2 {
  r3 := 0
  r3 := r2 = r3
  if r3 goto L5
  r3 := globals["sub"]
  r4 := cdr r1
  r5 := 1
  r5 := r2 - r5
  r3 := tailcall r5
  goto L6
  def L5
  r0 := car r1
  return r0
  def L6
}
globals["sub"] := r3
loadfunc r3 2 {
  r3 := globals["sub"]
  r4 := globals["L"]
  mov r5 r1
  r3 := r3 call r5
  r4 := globals["update"]
  r5 := globals["L"]
  mov r6 r1
  r7 := globals["sub"]
  r8 := globals["L"]
  mov r9 r2
  r7 := r7 call r9
  r4 := r4 call r7
  r4 := globals["update"]
  r5 := globals["L"]
  mov r6 r2
  mov r7 r3
  r4 := tailcall r7
}
globals["swap"] := r3
loadfunc r2 1 {
  r2 := 0
  r2 := r1 = r2
  if r2 goto L7
  r2 := 1
  r2 := r1 - r2
  r3 := '()
  r4 := globals["count"]
  r5 := 1
  r4 := r4 + r5
  globals["count"] := r4
  r4 := globals["permute"]
  mov r5 r2
  r4 := r4 call r5
  mov r3 r2
  goto L9
  def L10
  r4 := globals["swap"]
  mov r5 r2
  mov r6 r3
  r4 := r4 call r6
  r4 := globals["permute"]
  mov r5 r2
  r4 := r4 call r5
  r4 := globals["swap"]
  mov r5 r2
  mov r6 r3
  r4 := r4 call r6
  r4 := 1
  r3 := r3 - r4
  def L9
  r4 := globals[">="]
  mov r5 r3
  r6 := 0
  r4 := r4 call r6
  if r4 goto L10
  r0 := false
  return r0
  goto L8
  def L7
  r2 := globals["count"]
  r3 := 1
  r2 := r2 + r3
  globals["count"] := r2
  return r0
  def L8
}
globals["permute"] := r2
r0 := globals["permute"]
r1 := globals["N"]
r0 := r0 call r1
r0 := globals["count"]
check r0 "count"
r0 := 6235301
expect r0 "6235301"
