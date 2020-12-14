(* Project disambiguated VScheme into KNormal representation. 
    Note that this can fail if the disambiguated VScheme is not already 
    written in KNormal-form. *)

(* You'll complete this file *)

structure ProjectKN :> sig
  val value : UnambiguousVScheme.value -> KNormalForm.literal
  val def   : UnambiguousVScheme.def -> string KNormalForm.exp Error.error
end 
  = 
struct
  structure K  = KNormalForm
  structure KU = KNormalUtil
  structure P  = Primitive
  structure X  = UnambiguousVScheme

  infix 0 >>=   val op >>= = Error.>>=
  infix  3 <*>  val op <*> = Error.<*>
  infixr 4 <$>  val op <$> = Error.<$>

  val succeed = Error.succeed
  val error = Error.ERROR
  val errorList = Error.list

  (* val t = "$t1" *)

  fun fst (x,_) = x
  fun snd (_, y) = y
  fun curry  f x y   = f (x, y)
  fun curry3 f x y z = f (x, y, z)

  fun checky p = P.name p = "check" orelse P.name p = "expect"

  val asName : X.exp -> X.name Error.error
         (* project into a name; used where KNF expects a name *)
    = fn X.LOCAL x => succeed x 
       | e => error ("expected a local variable but instead got " ^ (X.whatIs e))
  
  val asString 
    = fn l1::X.LITERAL v::[] => succeed (ObjectCode.STRING (X.valToString v))
       | l1::e::[] => error ("expected a literal but instead got " ^ (X.whatIs e))
       | _ => error ("Value can not be converted to a string")
  (* fun checkyToKNormal (l1::l2::[]) =  succeed [(asName l1)] <*> (asString l2) 
    | checkyToKNormal ls = error ("Expected Checky form to have exactly two arguments") *)
  fun checkyBody (l1::l2::[]) = (Error.mapList asName [l1])
    | checkyBody _ = error ("Expected a Checky Body")

  fun value (X.SYM s) = K.STRING s
    | value (X.NUM n) = K.INT n
    | value (X.BOOLV b) = K.BOOL b
    | value (X.EMPTYLIST) = K.EMPTYLIST

  (* val _ : UnambiguousVScheme.exp -> string KNormalForm.exp Error.error *)
  fun exp (X.LITERAL v) = succeed (K.LIT (value v))
    | exp (X.LOCAL n) = succeed (K.NAME n)
    | exp (X.GLOBAL n) = succeed (KU.getglobal n)
    | exp (X.SETLOCAL (n, e))  = (curry (K.ASSIGN) n) <$> (K.NAME <$> (asName e))
    | exp (X.SETGLOBAL (n, e)) = (curry (KU.setglobal)) <$> asName e <*> succeed (n)
    | exp (X.IFX (e1, e2, e3)) =  (curry3 K.IF_THEN_ELSE) <$> asName e1 <*> exp e2 <*> exp e3
    | exp (X.WHILEX (X.LETX (X.LET, xs, X.GLOBAL n), e2)) = if n = (fst (hd xs)) 
                                                           then (curry3 (K.WHILE)) (fst (hd xs)) <$> exp ((snd o hd) xs) <*> exp e2
                                                           else error "While let binding ill formed"
    
    | exp (X.WHILEX (X.LETX (X.LET, xs, X.LOCAL n), e2)) = if n = (fst (hd xs)) 
                                                  then (curry3 (K.WHILE)) (fst (hd xs)) <$> exp ((snd o hd) xs) <*> exp e2
                                                  else error "While let binding ill formed" 
    | exp (X.BEGIN (e1::e2::[])) = curry (K.SEQ) <$> exp e1 <*> exp e2 
    | exp (X.FUNCALL (e, l)) = curry (K.FUNCALL) <$> asName e <*> (Error.mapList asName l)
    | exp (X.PRIMCALL (p, l)) = (case checky p of
                                   true => (curry3 (K.VMOP_LIT)) p <$> checkyBody l <*> asString l
                                 | false => (curry (K.VMOP)) p <$> (Error.mapList asName l))
    | exp (X.LETX (X.LET, x::[], e)) = (curry3 (K.LET)) (fst x) <$> exp (snd x) <*> exp e
    | exp e = Impossible.impossible (X.whatIs e)

  fun def (X.EXP (X.LETX (X.LET, (t, X.LAMBDA (nl, e))::[], (X.SETGLOBAL (name, t'))))) = let 
                                                                      val t' = (case t' of X.LOCAL x => x | X.GLOBAL x => x | _ => Impossible.impossible "not a name")
                                                                      fun fundef e = if t' = t 
                                                                                     then succeed ((curry3 (K.LET)) t (K.FUNCODE (nl, e)) (KU.setglobal (name, t))) 
                                                                                     else error "set global and let binding have to be same"
                                                                      in (exp e) >>= fundef
                                                                      end
    | def (X.EXP e) = exp e
    | def (X.DEFINE (n, l)) = def (X.EXP (X.LETX (X.LET, (n, X.LAMBDA l)::[], (X.SETGLOBAL (n, X.GLOBAL n)))))
    | def _         = Impossible.impossible "no way"

end

