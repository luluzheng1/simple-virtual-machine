loadfunc r0 1 {
  r2 := 0
  r2 := r1 eq r2
  if r2 
  goto L1
    r2 := 1
    r1 := r1 - r2
    tailcall r0 (r1)
  def L1
    return r1
}

globals["big"] := r0
r1 := 555

r0 := call r0 (r1)