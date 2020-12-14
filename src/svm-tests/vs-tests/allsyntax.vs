loadfunc r1 3 {
    zero r2
    r3 := 3
    r2 := r2 + r3
    check  r3 "0 + 3 = 3"
    expect r2 "3"
    r4 := "You passed a test"
    print r4
    r2 := Bool(r2)
    if r2
    goto end
    r3 := "Shouldn't print"
    print r3
    halt
    def end
    r3 := "Should print"
    print r3
    globals["hello"] := r3
    r3 := globals["hello"]
    print r3
    r1 := r1 eq r2 
    r1 := r1 > r2
    r1 := r1 < r2
    r1 := r1 idiv r2 
    r1 := r1 / r2
    r1 := r1 * r2
    r1 := function? r1
    r1 := symbol? r1
    r1 := pair? r1
    r1 := number? r1
    r1 := boolean? r1
    r1 := null? r1
    r1 := nil? r1
    r1 := car r1
    r1 := cdr r1
    r1 := hash r1
    error r1
    printu r1
    println r1
}