(* Closure conversion from unambiguous VScheme to Closed Scheme. 
    This is where we handle lambda and captured variables *)

(* You'll write this file *)

structure ClosureConvert :> sig
  val close : UnambiguousVScheme.def -> ClosedScheme.def
end 
  = 
struct
  structure X = UnambiguousVScheme
  structure C = ClosedScheme
  structure S = Set

  fun literal (X.SYM x)   = C.STRING x
    | literal (X.NUM i)   = C.INT i
    | literal (X.BOOLV b) = C.BOOL b
    | literal X.EMPTYLIST = C.EMPTYLIST

  fun fst (x, _) = x
  fun snd (_, y) = y

  fun indexOf x xs = 
    (* returns `SOME i`, where i is the position of `x` in `xs`,
       or if `x` does not appear in `xs`, returns `NONE` *)
    let fun find k []        = NONE 
          | find k (y :: ys) = if x = y then SOME k else find (k + 1) ys
    in  find 0 xs
    end

  fun singleton n = S.insert (n, S.empty)

  fun free (X.LITERAL v) = S.empty
    | free (X.LOCAL n) = singleton n
    | free (X.GLOBAL n) = S.empty
    | free (X.SETLOCAL (n, e)) = S.union' [(singleton n), free e]
    | free (X.SETGLOBAL (n, e)) = free e
    | free (X.IFX (e1, e2, e3)) = S.union' [free e1, free e2, free e3]
    | free (X.WHILEX (e, e')) = S.union' [free e, free e']
    | free (X.BEGIN (es)) = S.union' (List.map free es) 
    | free (X.FUNCALL (e, es)) = S.union' (List.map free (e::es)) 
    | free (X.PRIMCALL (p, es)) = S.union' (List.map free es) 
    | free (X.LETX (X.LET, l, e)) = 
      let val (names, es) = ListPair.unzip l
          val s = List.foldl (fn (e', acc) => S.union' [free e', acc]) S.empty es
      in S.union' [s, S.diff(free e, S.ofList names)]
      end   
    | free (X.LETX (X.LETREC, l, e)) = 
      let val (names, es) = ListPair.unzip l
          val free_es = map free es
          val name_diff = S.diff(S.union' free_es, S.ofList names)
          val free_body = S.diff (free e, S.ofList names)
      in
        S.union' [free_body, name_diff]
      end
    | free (X.LAMBDA (nl, e)) = S.diff (free e, S.ofList nl)
    
  val free : X.exp -> X.name S.set 
    = free
    
  fun freeOfLambda xs body = free (X.LAMBDA (xs,body))

  fun unLambda (X.LAMBDA lambda) = lambda
    | unLambda _ = Impossible.impossible "parser failed to insist on a lambda"

  fun closeExp captured e =
    (* Given an expression `e` in Unambiguous vScheme, plus a list
       of the free variables of that expression, return the closure-
       converted version of the expression in Closed Scheme *)
    let val _ = closeExp : X.name list -> X.exp -> C.exp

        (* I recommend internal function closure : X.lambda -> C.closure *)
        fun closure (xs, body) = 
          let val c_set = S.ofList xs
              val free_vars = freeOfLambda xs body
              val new_captured_names = S.elems(S.diff(free_vars, c_set))
          in ((xs, closeExp new_captured_names body), (map ((closeExp captured) o X.LOCAL) new_captured_names))
          end
        val _ = closure : X.lambda -> C.closure

        (* I recommend internal function exp : X.exp -> C.exp *)
        fun exp (X.LITERAL v)              = C.LITERAL (literal v)
          | exp (X.LOCAL n)                = snd (foldl (fn (x, (i, v)) => if x = n 
                                                                 then (i + 1, C.CAPTURED i)
                                                                 else (i+1, v)) (0, C.LOCAL n) captured)
          | exp (X.GLOBAL n)               = C.GLOBAL n
          | exp (X.SETLOCAL (n, e))        = if List.exists (fn (x) => x = n) captured
                                             then Impossible.impossible "Can not write a capture variable"
                                             else C.SETLOCAL (n, exp e)
          | exp (X.SETGLOBAL (n, e))       = C.SETGLOBAL (n, exp e)
          | exp (X.IFX (e1, e2, e3))       = C.IFX (exp e1, exp e2, exp e3)
          | exp (X.WHILEX (e, e'))         = C.WHILEX (exp e, exp e')
          | exp (X.BEGIN (es))             = C.BEGIN(map exp es)
          | exp (X.FUNCALL (e, es))        = C.FUNCALL(exp e, map exp es)
          | exp (X.PRIMCALL (p, es))       = C.PRIMCALL(p, map exp es)
          | exp (X.LETX (X.LET, ls, e))    = C.LET (map (fn (n,e) => (n, exp e)) ls, exp e) 
          | exp (X.LETX (X.LETREC, ls, e)) = C.LETREC ((map (fn (n, e) => (n, closure(unLambda e))) ls), exp e)
          | exp (X.LAMBDA (nl, e))         = C.CLOSURE (closure(nl, e))
    in  exp e
    end

  fun close (X.VAL (n, e)) = C.VAL (n, closeExp [] e)
    | close (X.DEFINE (n, (nl, e))) = C.DEFINE (n, (nl, closeExp [] e))
    | close (X.EXP e) = C.EXP(closeExp [] e)
    | close (X.CHECK_EXPECT (s1, e1, s2, e2)) = C.CHECK_EXPECT(s1, closeExp [] e1, s2, closeExp [] e2)
    | close (X.CHECK_ASSERT (s, e)) = C.CHECK_ASSERT(s, closeExp [] e)


end
