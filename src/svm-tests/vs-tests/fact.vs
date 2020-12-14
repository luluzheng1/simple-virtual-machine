loadfunc r0 1 {
r2 := 0
r2 := r1 = r2
if r2 goto L1
r2 := globals["factorial"]
r3 := 1
r3 := r1 - r3
r2 := r2 call r3
r0 := r1 * r2
return r0
goto L2
def L1
r0 := 1
return r0
def L2
}
globals["factorial"] := r0
r0 := globals["factorial"]
r1 := 4
r0 := r0 call r1
check r0 "4-factorial"
r0 := 24
expect r0 "24"
