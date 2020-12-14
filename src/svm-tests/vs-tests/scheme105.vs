loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L1
  r3 := globals["equal?"]
  mov r4 r1
  r2 := car r5
  r3 := r3 call r5
  if r3 goto L3
  r3 := globals["count"]
  mov r4 r1
  r2 := cdr r5
  r5 := tailcall r3
  goto L4
  def L3
  r3 := 1
  r4 := globals["count"]
  mov r5 r1
  r2 := cdr r6
  r4 := r4 call r6
  r0 := r3 + r4
  return r0
  def L4
  goto L2
  def L1
  r0 := 0
  return r0
  def L2
}
globals["count"] := r3
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L5
  r3 := globals["atom?"]
  r2 := car r4
  r3 := r3 call r4
  if r3 goto L7
  r3 := globals["count"]
  mov r4 r1
  r2 := car r5
  r3 := r3 call r5
  r4 := globals["countall"]
  mov r5 r1
  r2 := cdr r6
  r4 := r4 call r6
  r0 := r3 + r4
  return r0
  goto L8
  def L7
  r3 := globals["equal?"]
  mov r4 r1
  r2 := car r5
  r3 := r3 call r5
  if r3 goto L9
  r3 := globals["countall"]
  mov r4 r1
  r2 := cdr r5
  r5 := tailcall r3
  goto L10
  def L9
  r3 := 1
  r4 := globals["countall"]
  mov r5 r1
  r2 := cdr r6
  r4 := r4 call r6
  r0 := r3 + r4
  return r0
  def L10
  def L8
  goto L6
  def L5
  r0 := 0
  return r0
  def L6
}
globals["countall"] := r3
loadfunc r2 1 {
  r1 := null? r2
  if r2 goto L11
  r2 := globals["atom?"]
  r1 := car r3
  r2 := r2 call r3
  if r2 goto L13
  r2 := globals["append"]
  r3 := globals["mirror"]
  r1 := cdr r4
  r3 := r3 call r4
  r4 := globals["list1"]
  r5 := globals["reverse"]
  r1 := car r6
  r5 := r5 call r6
  r4 := r4 call r5
  r4 := tailcall r2
  goto L14
  def L13
  r2 := globals["append"]
  r3 := globals["mirror"]
  r1 := cdr r4
  r3 := r3 call r4
  r4 := globals["list1"]
  r1 := car r5
  r4 := r4 call r5
  r4 := tailcall r2
  def L14
  goto L12
  def L11
  r0 := '()
  return r0
  def L12
}
globals["mirror"] := r2
loadfunc r2 1 {
  r1 := null? r2
  if r2 goto L15
  r2 := globals["atom?"]
  r1 := car r3
  r2 := r2 call r3
  if r2 goto L17
  r2 := globals["append"]
  r3 := globals["flatten"]
  r1 := car r4
  r3 := r3 call r4
  r4 := globals["flatten"]
  r1 := cdr r5
  r4 := r4 call r5
  r4 := tailcall r2
  goto L18
  def L17
  r1 := car r2
  r3 := globals["flatten"]
  r1 := cdr r4
  r3 := r3 call r4
  r0 := r2 cons r3
  return r0
  def L18
  goto L16
  def L15
  r0 := '()
  return r0
  def L16
}
globals["flatten"] := r2
loadfunc r4 3 {
  r1 := null? r4
  if r4 goto L19
  r4 := globals["equal?"]
  r1 := car r5
  r2 := car r6
  r4 := r4 call r6
  if r4 goto L21
  if r3 goto L23
  r4 := globals["find-contig"]
  mov r5 r1
  r2 := cdr r6
  r7 := false
  r7 := tailcall r4
  goto L24
  def L23
  r0 := false
  return r0
  def L24
  goto L22
  def L21
  r4 := globals["find-contig"]
  r1 := cdr r5
  r2 := cdr r6
  r7 := true
  r7 := tailcall r4
  def L22
  goto L20
  def L19
  return r3
  def L20
}
globals["find-contig"] := r4
loadfunc r3 2 {
  r1 := null? r3
  if r3 goto L25
  r2 := null? r3
  if r3 goto L27
  r3 := globals["find-contig"]
  mov r4 r1
  mov r5 r2
  r6 := false
  r6 := tailcall r3
  goto L28
  def L27
  r0 := false
  return r0
  def L28
  goto L26
  def L25
  r0 := true
  return r0
  def L26
}
globals["contig-sublist?"] := r3
loadfunc r4 3 {
  r1 := null? r4
  if r4 goto L29
  r2 := null? r4
  if r4 goto L31
  r4 := globals["equal?"]
  r1 := car r5
  r2 := car r6
  r4 := r4 call r6
  if r4 goto L33
  r4 := globals["find-subset"]
  mov r5 r1
  r2 := cdr r6
  mov r7 r3
  r7 := tailcall r4
  goto L34
  def L33
  r4 := globals["find-subset"]
  r1 := cdr r5
  r2 := cdr r6
  r7 := true
  r7 := tailcall r4
  def L34
  goto L32
  def L31
  r0 := false
  return r0
  def L32
  goto L30
  def L29
  return r3
  def L30
}
globals["find-subset"] := r4
loadfunc r3 2 {
  r3 := globals["find-subset"]
  mov r4 r1
  mov r5 r2
  r6 := false
  r6 := tailcall r3
}
globals["sublist?"] := r3
loadfunc r2 1 {
  r2 := globals["mod"]
  mov r3 r1
  r4 := 2
  r2 := r2 call r4
  r3 := 0
  r0 := r2 = r3
  return r0
}
globals["even?"] := r2
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L35
  mov r3 r1
  r2 := car r4
  r3 := r3 call r4
  if r3 goto L37
  r0 := '()
  return r0
  goto L38
  def L37
  r2 := car r3
  r4 := globals["takewhile"]
  mov r5 r1
  r2 := cdr r6
  r4 := r4 call r6
  r0 := r3 cons r4
  return r0
  def L38
  goto L36
  def L35
  return r2
  def L36
}
globals["takewhile"] := r3
loadfunc r3 2 {
  r2 := null? r3
  if r3 goto L39
  mov r3 r1
  r2 := car r4
  r3 := r3 call r4
  if r3 goto L41
  return r2
  goto L42
  def L41
  r3 := globals["dropwhile"]
  mov r4 r1
  r2 := cdr r5
  r5 := tailcall r3
  def L42
  goto L40
  def L39
  return r2
  def L40
}
globals["dropwhile"] := r3
loadfunc r3 2 {
  r1 := null? r3
  if r3 goto L43
  r3 := globals["list2"]
  r1 := car r4
  r2 := car r5
  r3 := r3 call r5
  r4 := globals["zip"]
  r1 := cdr r5
  r2 := cdr r6
  r4 := r4 call r6
  r0 := r3 cons r4
  return r0
  goto L44
  def L43
  r0 := '()
  return r0
  def L44
}
globals["zip"] := r3
loadfunc r3 2 {
  r2 := cdr r3
  r3 := null? r3
  if r3 goto L45
  mov r3 r1
  r2 := car r4
  r3 := r3 call r4
  mov r4 r1
  r5 := globals["cadr"]
  mov r6 r2
  r5 := r5 call r6
  r4 := r4 call r5
  r3 := r3 <= r4
  if r3 goto L47
  r3 := globals["arg-max"]
  r2 := car r4
  r5 := globals["cddr"]
  mov r6 r2
  r5 := r5 call r6
  r4 := r4 cons r5
  r4 := tailcall r3
  goto L48
  def L47
  r3 := globals["arg-max"]
  r2 := cdr r4
  r4 := tailcall r3
  def L48
  goto L46
  def L45
  return r2
  def L46
}
globals["arg-max"] := r3
loadfunc r3 2 {
  r1 := null? r3
  if r3 goto L49
  r2 := null? r3
  if r3 goto L51
  r1 := car r3
  r2 := car r4
  r3 := r3 <= r4
  if r3 goto L53
  r3 := globals["merge"]
  r2 := car r4
  r4 := r4 cons r1
  r2 := cdr r5
  r5 := tailcall r3
  goto L54
  def L53
  r1 := car r3
  r4 := globals["merge"]
  r1 := cdr r5
  mov r6 r2
  r4 := r4 call r6
  r0 := r3 cons r4
  return r0
  def L54
  goto L52
  def L51
  return r1
  def L52
  goto L50
  def L49
  r0 := r1 cons r2
  return r0
  def L50
}
globals["merge"] := r3
loadfunc r4 3 {
  r1 := null? r4
  if r4 goto L55
  r4 := globals["list-splitter"]
  r1 := cdr r5
  r1 := car r6
  r6 := r6 cons r3
  mov r7 r2
  r7 := tailcall r4
  goto L56
  def L55
  r4 := '()
  r4 := r2 cons r4
  r0 := r3 cons r4
  return r0
  def L56
}
globals["list-splitter"] := r4
loadfunc r2 1 {
  r2 := globals["list-splitter"]
  mov r3 r1
  r4 := '()
  r5 := '()
  r5 := tailcall r2
}
globals["split-list"] := r2
r0 := globals["count"]
r0 := function? r0
check r0 "(function? count)"
r0 := true
expect r0 "#t"
r0 := globals["countall"]
r0 := function? r0
check r0 "(function? countall)"
r0 := true
expect r0 "#t"
r0 := globals["mirror"]
r0 := function? r0
check r0 "(function? mirror)"
r0 := true
expect r0 "#t"
r0 := globals["flatten"]
r0 := function? r0
check r0 "(function? flatten)"
r0 := true
expect r0 "#t"
r0 := globals["contig-sublist?"]
r0 := function? r0
check r0 "(function? contig-sublist?)"
r0 := true
expect r0 "#t"
r0 := globals["sublist?"]
r0 := function? r0
check r0 "(function? sublist?)"
r0 := true
expect r0 "#t"
r0 := globals["takewhile"]
r0 := function? r0
check r0 "(function? takewhile)"
r0 := true
expect r0 "#t"
r0 := globals["dropwhile"]
r0 := function? r0
check r0 "(function? dropwhile)"
r0 := true
expect r0 "#t"
r0 := globals["zip"]
r0 := function? r0
check r0 "(function? zip)"
r0 := true
expect r0 "#t"
r0 := globals["arg-max"]
r0 := function? r0
check r0 "(function? arg-max)"
r0 := true
expect r0 "#t"
r0 := globals["merge"]
r0 := function? r0
check r0 "(function? merge)"
r0 := true
expect r0 "#t"
r0 := globals["split-list"]
r0 := function? r0
check r0 "(function? split-list)"
r0 := true
expect r0 "#t"
r0 := globals["count"]
r1 := 5
r2 := 1
r3 := 2
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(count 5 '(1 2))"
r0 := 0
expect r0 "0"
r0 := globals["count"]
r1 := 1
r2 := 1
r3 := "a"
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(count 1 '(1 a))"
r0 := 1
expect r0 "1"
r0 := globals["count"]
r1 := 1
r2 := 1
r3 := "a"
r4 := 1
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(count 1 '(1 (a 1)))"
r0 := 1
expect r0 "1"
r0 := globals["countall"]
r1 := "a"
r2 := 1
r3 := 2
r4 := 3
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(countall 'a '(1 2 3))"
r0 := 0
expect r0 "0"
r0 := globals["countall"]
r1 := "a"
r2 := 1
r3 := "a"
r4 := 3
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(countall 'a '(1 a 3))"
r0 := 1
expect r0 "1"
r0 := globals["countall"]
r1 := "a"
r2 := "a"
r3 := 1
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r3 := 3
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(countall 'a '((a 1) 3))"
r0 := 1
expect r0 "1"
r0 := globals["countall"]
r1 := "a"
r2 := "a"
r3 := 1
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r3 := "a"
r4 := 2
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r4 := 3
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(countall 'a '((a 1) (a 2) 3))"
r0 := 2
expect r0 "2"
r0 := globals["countall"]
r1 := "a"
r2 := "a"
r3 := 1
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r3 := "a"
r4 := "a"
r5 := 2
r6 := '()
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r4 := 3
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(countall 'a '((a 1) (a a 2) 3))"
r0 := 3
expect r0 "3"
r0 := globals["mirror"]
r1 := '()
r0 := r0 call r1
check r0 "(mirror '())"
r0 := '()
expect r0 "'()"
r0 := globals["mirror"]
r1 := 3
r2 := 2
r3 := 1
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 call r1
check r0 "(mirror '(3 2 1))"
r0 := 1
r1 := 2
r2 := 3
r3 := '()
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'(1 2 3)"
r0 := globals["mirror"]
r1 := 3
r2 := 2
r3 := 1
r4 := 4
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 call r1
check r0 "(mirror '(3 2 (1 4)))"
r0 := 4
r1 := 1
r2 := '()
r1 := r1 cons r2
r0 := r0 cons r1
r1 := 2
r2 := 3
r3 := '()
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'((4 1) 2 3)"
r0 := globals["flatten"]
r1 := '()
r0 := r0 call r1
check r0 "(flatten '())"
r0 := '()
expect r0 "'()"
r0 := globals["flatten"]
r1 := "a"
r2 := '()
r1 := r1 cons r2
r2 := "b"
r3 := '()
r2 := r2 cons r3
r3 := '()
r2 := r2 cons r3
r3 := '()
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 call r1
check r0 "(flatten '((a) ((b))))"
r0 := "a"
r1 := "b"
r2 := '()
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'(a b)"
r0 := globals["contig-sublist?"]
r1 := "a"
r2 := "b"
r3 := "c"
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r2 := "x"
r3 := "y"
r4 := "a"
r5 := "b"
r6 := "c"
r7 := "m"
r8 := "n"
r9 := '()
r8 := r8 cons r9
r7 := r7 cons r8
r6 := r6 cons r7
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(contig-sublist? '(a b c) '(x y a b c m n))"
r0 := true
expect r0 "#t"
r0 := globals["contig-sublist?"]
r1 := "a"
r2 := "b"
r3 := "c"
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r2 := "x"
r3 := "f"
r4 := "a"
r5 := "y"
r6 := "b"
r7 := "c"
r8 := "m"
r9 := "n"
r10 := '()
r9 := r9 cons r10
r8 := r8 cons r9
r7 := r7 cons r8
r6 := r6 cons r7
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(contig-sublist? '(a b c) '(x f a y b c m n))"
r0 := false
expect r0 "#f"
r0 := globals["sublist?"]
r1 := "a"
r2 := "b"
r3 := "c"
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r2 := "x"
r3 := "y"
r4 := "a"
r5 := "z"
r6 := "b"
r7 := "c"
r8 := "m"
r9 := "n"
r10 := '()
r9 := r9 cons r10
r8 := r8 cons r9
r7 := r7 cons r8
r6 := r6 cons r7
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(sublist? '(a b c) '(x y a z b c m n))"
r0 := true
expect r0 "#t"
r0 := globals["sublist?"]
r1 := "a"
r2 := "b"
r3 := "c"
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r2 := "x"
r3 := "f"
r4 := "a"
r5 := "y"
r6 := "b"
r7 := "m"
r8 := "n"
r9 := '()
r8 := r8 cons r9
r7 := r7 cons r8
r6 := r6 cons r7
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(sublist? '(a b c) '(x f a y b m n))"
r0 := false
expect r0 "#f"
r0 := globals["takewhile"]
r1 := globals["even?"]
r2 := 2
r3 := 4
r4 := 6
r5 := 7
r6 := 8
r7 := 10
r8 := 12
r9 := '()
r8 := r8 cons r9
r7 := r7 cons r8
r6 := r6 cons r7
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(takewhile even? '(2 4 6 7 8 10 12))"
r0 := 2
r1 := 4
r2 := 6
r3 := '()
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'(2 4 6)"
r0 := globals["dropwhile"]
r1 := globals["even?"]
r2 := 2
r3 := 4
r4 := 6
r5 := 7
r6 := 8
r7 := 10
r8 := 12
r9 := '()
r8 := r8 cons r9
r7 := r7 cons r8
r6 := r6 cons r7
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(dropwhile even? '(2 4 6 7 8 10 12))"
r0 := 7
r1 := 8
r2 := 10
r3 := 12
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'(7 8 10 12)"
r0 := globals["zip"]
r1 := 1
r2 := 2
r3 := 3
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r2 := "a"
r3 := "b"
r4 := "c"
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(zip '(1 2 3) '(a b c))"
r0 := 1
r1 := "a"
r2 := '()
r1 := r1 cons r2
r0 := r0 cons r1
r1 := 2
r2 := "b"
r3 := '()
r2 := r2 cons r3
r1 := r1 cons r2
r2 := 3
r3 := "c"
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r3 := '()
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'((1 a) (2 b) (3 c))"
r0 := globals["merge"]
r1 := 4
r2 := 7
r3 := 9
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r2 := 5
r3 := 6
r4 := 8
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r0 := r0 call r2
check r0 "(merge '(4 7 9) '(5 6 8))"
r0 := 4
r1 := 5
r2 := 6
r3 := 7
r4 := 8
r5 := 9
r6 := '()
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'(4 5 6 7 8 9)"
r0 := globals["merge"]
r1 := 4
r2 := 5
r3 := '()
r2 := r2 cons r3
r1 := r1 cons r2
r2 := '()
r0 := r0 call r2
check r0 "(merge '(4 5) '())"
r0 := 4
r1 := 5
r2 := '()
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'(4 5)"
r0 := globals["split-list"]
r1 := "a"
r2 := "b"
r3 := "c"
r4 := "d"
r5 := '()
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 call r1
check r0 "(split-list '(a b c d))"
r0 := "c"
r1 := "a"
r2 := '()
r1 := r1 cons r2
r0 := r0 cons r1
r1 := "d"
r2 := "b"
r3 := '()
r2 := r2 cons r3
r1 := r1 cons r2
r2 := '()
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'((c a) (d b))"
r0 := globals["split-list"]
r1 := "a"
r2 := "b"
r3 := "c"
r4 := "d"
r5 := "e"
r6 := '()
r5 := r5 cons r6
r4 := r4 cons r5
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r0 := r0 call r1
check r0 "(split-list '(a b c d e))"
r0 := "d"
r1 := "b"
r2 := '()
r1 := r1 cons r2
r0 := r0 cons r1
r1 := "e"
r2 := "c"
r3 := "a"
r4 := '()
r3 := r3 cons r4
r2 := r2 cons r3
r1 := r1 cons r2
r2 := '()
r1 := r1 cons r2
r0 := r0 cons r1
expect r0 "'((d b) (e c a))"
