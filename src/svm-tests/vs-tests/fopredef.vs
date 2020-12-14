loadfunc r2 1 {
  r1 := car r2
  r2 := car r0
  return r0
}
globals["caar"] := r2
loadfunc r2 1 {
  r1 := cdr r2
  r2 := car r0
  return r0
}
globals["cadr"] := r2
loadfunc r2 1 {
  r1 := car r2
  r2 := cdr r0
  return r0
}
globals["cdar"] := r2
loadfunc r2 1 {
  r1 := cdr r2
  r2 := cdr r0
  return r0
}
globals["cddr"] := r2
loadfunc r2 1 {
  r2 := globals["caar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["caaar"] := r2
loadfunc r2 1 {
  r2 := globals["cadr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["caadr"] := r2
loadfunc r2 1 {
  r2 := globals["cdar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["cadar"] := r2
loadfunc r2 1 {
  r2 := globals["cddr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["caddr"] := r2
loadfunc r2 1 {
  r2 := globals["caar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cdaar"] := r2
loadfunc r2 1 {
  r2 := globals["cadr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cdadr"] := r2
loadfunc r2 1 {
  r2 := globals["cdar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cddar"] := r2
loadfunc r2 1 {
  r2 := globals["cddr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cdddr"] := r2
loadfunc r2 1 {
  r2 := globals["caaar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["caaaar"] := r2
loadfunc r2 1 {
  r2 := globals["caadr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["caaadr"] := r2
loadfunc r2 1 {
  r2 := globals["cadar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["caadar"] := r2
loadfunc r2 1 {
  r2 := globals["caddr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["caaddr"] := r2
loadfunc r2 1 {
  r2 := globals["cdaar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["cadaar"] := r2
loadfunc r2 1 {
  r2 := globals["cdadr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["cadadr"] := r2
loadfunc r2 1 {
  r2 := globals["cddar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["caddar"] := r2
loadfunc r2 1 {
  r2 := globals["cdddr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := car r0
  return r0
}
globals["cadddr"] := r2
loadfunc r2 1 {
  r2 := globals["caaar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cdaaar"] := r2
loadfunc r2 1 {
  r2 := globals["caadr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cdaadr"] := r2
loadfunc r2 1 {
  r2 := globals["cadar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cdadar"] := r2
loadfunc r2 1 {
  r2 := globals["caddr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cdaddr"] := r2
loadfunc r2 1 {
  r2 := globals["cdaar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cddaar"] := r2
loadfunc r2 1 {
  r2 := globals["cdadr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cddadr"] := r2
loadfunc r2 1 {
  r2 := globals["cddar"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cdddar"] := r2
loadfunc r2 1 {
  r2 := globals["cdddr"]
  mov r3 r1
  r2 := r2 call r3
  r2 := cdr r0
  return r0
}
globals["cddddr"] := r2
loadfunc r2 1 {
  r2 := '()
  r0 := r1 cons r2
  return r0
}
globals["list1"] := r2
loadfunc r3 2 {
  r3 := globals["list1"]
  mov r4 r2
  r3 := r3 call r4
  r0 := r1 cons r3
  return r0
}
globals["list2"] := r3
loadfunc r4 3 {
  r4 := globals["list2"]
  mov r5 r2
  mov r6 r3
  r4 := r4 call r6
  r0 := r1 cons r4
  return r0
}
globals["list3"] := r4
loadfunc r3 2 {
  r1 := null? r3
  if r3 goto L1
  r1 := car r3
  r4 := globals["append"]
  r1 := cdr r5
  mov r6 r2
  r4 := r4 call r6
  r0 := r3 cons r4
  return r0
  goto L2
  def L1
  return r2
  def L2
}
globals["append"] := r3
loadfunc r3 2 {
  r1 := null? r3
  if r3 goto L3
  r3 := globals["revapp"]
  r1 := cdr r4
  r1 := car r5
  r5 := r5 cons r2
  r5 := tailcall r3
  goto L4
  def L3
  return r2
  def L4
}
globals["revapp"] := r3
loadfunc r2 1 {
  r2 := globals["revapp"]
  mov r3 r1
  r4 := '()
  r4 := tailcall r2
}
globals["reverse"] := r2
loadfunc r3 2 {
  if r1 goto L5
  return r1
  goto L6
  def L5
  return r2
  def L6
}
globals["and"] := r3
loadfunc r3 2 {
  if r1 goto L7
  return r2
  goto L8
  def L7
  return r1
  def L8
}
globals["or"] := r3
loadfunc r2 1 {
  if r1 goto L9
  r0 := true
  return r0
  goto L10
  def L9
  r0 := false
  return r0
  def L10
}
globals["not"] := r2
loadfunc r2 1 {
  r2 := globals["or"]
  r1 := symbol? r3
  r4 := globals["or"]
  r1 := number? r5
  r6 := globals["or"]
  r1 := boolean? r7
  r1 := null? r8
  r6 := r6 call r8
  r4 := r4 call r6
  r4 := tailcall r2
}
globals["atom?"] := r2
loadfunc r3 2 {
  r3 := globals["atom?"]
  mov r4 r1
  r3 := r3 call r4
  if r3 goto L11
  r3 := globals["atom?"]
  mov r4 r2
  r3 := r3 call r4
  if r3 goto L13
  r3 := globals["and"]
  r4 := globals["equal?"]
  r1 := car r5
  r2 := car r6
  r4 := r4 call r6
  r5 := globals["equal?"]
  r1 := cdr r6
  r2 := cdr r7
  r5 := r5 call r7
  r5 := tailcall r3
  goto L14
  def L13
  r0 := false
  return r0
  def L14
  goto L12
  def L11
  r0 := r1 = r2
  return r0
  def L12
}
globals["equal?"] := r3
loadfunc r3 2 {
  r3 := globals["list2"]
  mov r4 r1
  mov r5 r2
  r5 := tailcall r3
}
globals["make-alist-pair"] := r3
loadfunc r2 1 {
  r1 := car r0
  return r0
}
globals["alist-pair-key"] := r2
loadfunc r2 1 {
  r2 := globals["cadr"]
  mov r3 r1
  r3 := tailcall r2
}
globals["alist-pair-attribute"] := r2
loadfunc r2 1 {
  r2 := globals["alist-pair-key"]
  r1 := car r3
  r3 := tailcall r2
}
globals["alist-first-key"] := r2
loadfunc r2 1 {
  r2 := globals["alist-pair-attribute"]
  r1 := car r3
  r3 := tailcall r2
}
globals["alist-first-attribute"] := r2
loadfunc r4 3 {
  r3 := null? r4
  if r4 goto L15
  r4 := globals["equal?"]
  mov r5 r1
  r6 := globals["alist-first-key"]
  mov r7 r3
  r6 := r6 call r7
  r4 := r4 call r6
  if r4 goto L17
  r3 := car r4
  r5 := globals["bind"]
  mov r6 r1
  mov r7 r2
  r3 := cdr r8
  r5 := r5 call r8
  r0 := r4 cons r5
  return r0
  goto L18
  def L17
  r4 := globals["make-alist-pair"]
  mov r5 r1
  mov r6 r2
  r4 := r4 call r6
  r3 := cdr r5
  r0 := r4 cons r5
  return r0
  def L18
  goto L16
  def L15
  r4 := globals["list1"]
  r5 := globals["make-alist-pair"]
  mov r6 r1
  mov r7 r2
  r5 := r5 call r7
  r5 := tailcall r4
  def L16
}
globals["bind"] := r4
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L19
  r3 := globals["equal?"]
  mov r4 r1
  r5 := globals["alist-first-key"]
  mov r6 r2
  r5 := r5 call r6
  r3 := r3 call r5
  if r3 goto L21
  r3 := globals["find"]
  mov r4 r1
  r2 := cdr r5
  r5 := tailcall r3
  goto L22
  def L21
  r3 := globals["alist-first-attribute"]
  mov r4 r2
  r4 := tailcall r3
  def L22
  goto L20
  def L19
  r0 := '()
  return r0
  def L20
}
globals["find"] := r3
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L23
  mov r3 r1
  r2 := car r4
  r3 := r3 call r4
  if r3 goto L25
  r3 := globals["filter"]
  mov r4 r1
  r2 := cdr r5
  r5 := tailcall r3
  goto L26
  def L25
  r2 := car r3
  r4 := globals["filter"]
  mov r5 r1
  r2 := cdr r6
  r4 := r4 call r6
  r0 := r3 cons r4
  return r0
  def L26
  goto L24
  def L23
  r0 := '()
  return r0
  def L24
}
globals["filter"] := r3
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L27
  mov r3 r1
  r2 := car r4
  r3 := r3 call r4
  r4 := globals["map"]
  mov r5 r1
  r2 := cdr r6
  r4 := r4 call r6
  r0 := r3 cons r4
  return r0
  goto L28
  def L27
  r0 := '()
  return r0
  def L28
}
globals["map"] := r3
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L29
  mov r3 r1
  r2 := car r4
  r3 := r3 call r4
  r3 := globals["app"]
  mov r4 r1
  r2 := cdr r5
  r5 := tailcall r3
  goto L30
  def L29
  r0 := false
  return r0
  def L30
}
globals["app"] := r3
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L31
  mov r3 r1
  r2 := car r4
  r3 := r3 call r4
  if r3 goto L33
  r3 := globals["exists?"]
  mov r4 r1
  r2 := cdr r5
  r5 := tailcall r3
  goto L34
  def L33
  r0 := true
  return r0
  def L34
  goto L32
  def L31
  r0 := false
  return r0
  def L32
}
globals["exists?"] := r3
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L35
  mov r3 r1
  r2 := car r4
  r3 := r3 call r4
  if r3 goto L37
  r0 := false
  return r0
  goto L38
  def L37
  r3 := globals["all?"]
  mov r4 r1
  r2 := cdr r5
  r5 := tailcall r3
  def L38
  goto L36
  def L35
  r0 := true
  return r0
  def L36
}
globals["all?"] := r3
loadfunc r4 3 {
  r3 := null? r4
  if r4 goto L39
  mov r4 r1
  r3 := car r5
  r6 := globals["foldr"]
  mov r7 r1
  mov r8 r2
  r3 := cdr r9
  r6 := r6 call r9
  r6 := tailcall r4
  goto L40
  def L39
  return r2
  def L40
}
globals["foldr"] := r4
loadfunc r4 3 {
  r3 := null? r4
  if r4 goto L41
  r4 := globals["foldl"]
  mov r5 r1
  mov r6 r1
  r3 := car r7
  mov r8 r2
  r6 := r6 call r8
  r3 := cdr r7
  r7 := tailcall r4
  goto L42
  def L41
  return r2
  def L42
}
globals["foldl"] := r4
r0 := 10
globals["newline"] := r0
r0 := 40
globals["left-round"] := r0
r0 := 32
globals["space"] := r0
r0 := 41
globals["right-round"] := r0
r0 := 59
globals["semicolon"] := r0
r0 := 123
globals["left-curly"] := r0
r0 := 39
globals["quotemark"] := r0
r0 := 125
globals["right-curly"] := r0
r0 := 91
globals["left-square"] := r0
r0 := 93
globals["right-square"] := r0
loadfunc r3 2 {
  r3 := globals["not"]
  r4 := r1 > r2
  r4 := tailcall r3
}
globals["<="] := r3
loadfunc r3 2 {
  r3 := globals["not"]
  r4 := r1 < r2
  r4 := tailcall r3
}
globals[">="] := r3
loadfunc r3 2 {
  r3 := globals["not"]
  r4 := r1 = r2
  r4 := tailcall r3
}
globals["!="] := r3
loadfunc r3 2 {
  r3 := r1 > r2
  if r3 goto L43
  return r2
  goto L44
  def L43
  return r1
  def L44
}
globals["max"] := r3
loadfunc r3 2 {
  r3 := r1 < r2
  if r3 goto L45
  return r2
  goto L46
  def L45
  return r1
  def L46
}
globals["min"] := r3
loadfunc r2 1 {
  r2 := 0
  r0 := r2 - r1
  return r0
}
globals["negated"] := r2
loadfunc r3 2 {
  r3 := r1 / r2
  r3 := r2 * r3
  r0 := r1 - r3
  return r0
}
globals["mod"] := r3
loadfunc r3 2 {
  r3 := 0
  r3 := r2 = r3
  if r3 goto L47
  r3 := globals["gcd"]
  mov r4 r2
  r5 := globals["mod"]
  mov r6 r1
  mov r7 r2
  r5 := r5 call r7
  r5 := tailcall r3
  goto L48
  def L47
  return r1
  def L48
}
globals["gcd"] := r3
loadfunc r3 2 {
  r3 := 0
  r3 := r1 = r3
  if r3 goto L49
  r3 := globals["gcd"]
  mov r4 r1
  mov r5 r2
  r3 := r3 call r5
  r3 := r2 / r3
  r0 := r1 * r3
  return r0
  goto L50
  def L49
  r0 := 0
  return r0
  def L50
}
globals["lcm"] := r3
loadfunc r5 4 {
  r5 := globals["list3"]
  mov r6 r2
  mov r7 r3
  mov r8 r4
  r5 := r5 call r8
  r0 := r1 cons r5
  return r0
}
globals["list4"] := r5
loadfunc r6 5 {
  r6 := globals["list4"]
  mov r7 r2
  mov r8 r3
  mov r9 r4
  mov r10 r5
  r6 := r6 call r10
  r0 := r1 cons r6
  return r0
}
globals["list5"] := r6
loadfunc r7 6 {
  r7 := globals["list5"]
  mov r8 r2
  mov r9 r3
  mov r10 r4
  mov r11 r5
  mov r12 r6
  r7 := r7 call r12
  r0 := r1 cons r7
  return r0
}
globals["list6"] := r7
loadfunc r8 7 {
  r8 := globals["list6"]
  mov r9 r2
  mov r10 r3
  mov r11 r4
  mov r12 r5
  mov r13 r6
  mov r14 r7
  r8 := r8 call r14
  r0 := r1 cons r8
  return r0
}
globals["list7"] := r8
loadfunc r9 8 {
  r9 := globals["list7"]
  mov r10 r2
  mov r11 r3
  mov r12 r4
  mov r13 r5
  mov r14 r6
  mov r15 r7
  mov r16 r8
  r9 := r9 call r16
  r0 := r1 cons r9
  return r0
}
globals["list8"] := r9
