loadfunc r2 1 {
  loadfunc r2 1 {
    r2 := r0.0
    r3 := r0.1
    loadfunc r0 1 {
      r2 := null? r1
      if r2 goto L9
      r2 := r0.0
      r3 := r0.2
      r2 := r2 call r3
      r3 := car r1
      r2 := r2 call r3
      if r2 goto L11
      r2 := car r1
      r3 := r0.1
      r4 := r0.2
      r3 := r3 call r4
      r4 := cdr r1
      r3 := r3 call r4
      r0 := r2 cons r3
      return r0
      goto L12
      def L11
      r2 := r0.2
      r0 := r2 cons r1
      return r0
      def L12
      goto L10
      def L9
      r2 := globals["list1"]
      r3 := r0.2
      r2 := tailcall r3
      def L10
    }
    r0 := closure[r0 , 3]
    r0.0 := r2
    r0.1 := r3
    r0.2 := r1
    return r0
  }
  r2 := closure[r2 , 2]
  loadfunc r3 1 {
    r2 := null? r1
    if r2 goto L13
    r2 := r0.0
    r3 := car r1
    r2 := r2 call r3
    r3 := r0.1
    r4 := cdr r1
    r3 := r3 call r4
    r2 := tailcall r3
    goto L14
    def L13
    r0 := '()
    return r0
    def L14
  }
  r3 := closure[r3 , 2]
  r2.0 := r1
  r2.1 := r2
  r3.0 := r2
  r3.1 := r3
  return r3
}
globals["mkInsertionSort"] := r2
loadfunc r3 2 {
  r3 := null? r1
  if r3 goto L15
  r3 := globals["revapp.nr"]
  r4 := cdr r1
  r5 := car r1
  r5 := r5 cons r2
  r3 := tailcall r5
  goto L16
  def L15
  return r2
  def L16
}
globals["revapp.nr"] := r3
loadfunc r2 1 {
  r2 := globals["revapp.nr"]
  mov r3 r1
  r4 := '()
  r2 := tailcall r4
}
globals["rev"] := r2
loadfunc r3 2 {
  r3 := 0
  r3 := r1 = r3
  if r3 goto L17
  r3 := globals["listN^"]
  r4 := 1
  r4 := r1 - r4
  r5 := r1 cons r2
  r3 := tailcall r5
  goto L18
  def L17
  return r2
  def L18
}
globals["listN^"] := r3
loadfunc r2 1 {
  r2 := globals["rev"]
  r3 := globals["listN^"]
  mov r4 r1
  r5 := '()
  r3 := r3 call r5
  r2 := tailcall r3
}
globals["listN"] := r2
r0 := globals["listN"]
r1 := 10
r0 := r0 call r1
globals["x"] := r0
