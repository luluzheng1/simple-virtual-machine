(* Embeds KNormal-form Scheme into VScheme. This cannot fail. *)

(* You'll complete this file *)

structure EmbedKN :> sig
  val value : KNormalForm.literal -> VScheme.value
  val def   : VScheme.name KNormalForm.exp -> VScheme.def
end 
  = 
struct
  structure K  = KNormalForm
  structure S  = VScheme
  structure SU = VSchemeUtils
  structure P  = Primitive

  fun let' x e' e = S.LETX (S.LET, [(x, e')], e)   (* useful helper *)

  fun setglobal (ObjectCode.STRING s) (v::[]) = S.SET (s , (S.VAR v))
    | setglobal _ _                           = Impossible.impossible "Should not have a Set Global in this form"

  fun getglobal (ObjectCode.STRING v) =  S.VAR (v)
    | getglobal _       = Impossible.impossible ("Wrong Usage of Get Global")

  fun value (K.INT  i)    = S.NUM i 
    | value (K.REAL r)    = S.NUM (Real.round r) 
    | value (K.STRING s)  = S.SYM s
    | value (K.BOOL b)    = S.BOOLV b
    | value (K.EMPTYLIST) = S.EMPTYLIST
    | value K.NIL         = S.BOOLV false 
  
  fun nameFrom (K.STRING s) = s
    | nameFrom _            = Impossible.impossible "uh oh"

  fun exp (K.LIT lit) = S.LITERAL (value lit)
    | exp (K.NAME  n) = S.VAR n
    | exp (K.VMOP (vmop, l)) = S.APPLY ((S.VAR (P.name vmop)), (List.map (S.VAR) l)) 
    | exp (K.VMOP_LIT (vmop, l, v)) = let 
                val name = P.name vmop
                val ret = (case name of 
                          "setglobal"  => setglobal v l
                        | "getglobal"  => getglobal v
                        |  s           =>  S.APPLY (S.VAR s, (List.map (S.VAR) l) @ [S.LITERAL(value v)])
                )
                in ret
                end
    | exp (K.FUNCALL (n, l)) = S.APPLY ((S.VAR n), (List.map (S.VAR) l) )
    | exp (K.IF_THEN_ELSE (x, e1, e2)) = S.IFX (S.VAR x, exp e1, exp e2)
    | exp (K.LET (x, e, e')) = S.LETX (S.LET, [(x, exp e)], exp e')
    | exp (K.SEQ (e1, e2)) = S.BEGIN [exp e1, exp e2]
    | exp (K.ASSIGN (x, e)) =  S.SET (x, exp e)
    | exp (K.WHILE (x, e, e')) = S.WHILEX (exp (K.LET (x, e, K.NAME x)), exp e')
    | exp (K.FUNCODE (l, e)) = S.LAMBDA (l, exp e)
    | exp (K.CAPTURED i) = SU.nth (i+1) (S.VAR "$closure")
    | exp (K.CLOSURE (formals, body, captured)) = 
        let
          val lambda = S.LAMBDA (("$closure"::formals), exp body)
          val cls = List.map (S.VAR) captured
        in
          SU.list (lambda::cls)
        end
    | exp (K.LETREC (ls, e)) = 
    let 
      val nx = map (fn (n,c) => (n, exp (K.CLOSURE c))) ls
    in S.LETX(S.LETREC, nx, exp e)
    end 
  fun def e = S.EXP (exp e) 

end
