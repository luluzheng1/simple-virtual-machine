(* KNormalizer from First-Order Scheme to KNormal-form Scheme. 
    This is where register allocation happens! *)

(* You'll fill out the missing code in this file *)

structure KNormalize :> sig
  type reg = int  (* register *)
  type regset     (* set of registers *)
  val regname : reg -> string
  val exp : reg Env.env -> regset -> ClosedScheme.exp -> reg KNormalForm.exp
  val def :                          ClosedScheme.def -> reg KNormalForm.exp
end 
  =
struct 
  structure K  = KNormalForm
  structure KU  = KNormalUtil
  structure C  = ClosedScheme
  structure E  = Env
  structure O  = ObjectCode
  structure P  = Primitive

  fun fst (x, y) = x
  fun snd (_, y) = y
  fun member x = List.exists (fn y => x = y)

  fun eprint s = TextIO.output (TextIO.stdErr, s)

  exception SetError of string

  (************ Register and regset operations ************)

  type reg = int
  fun regname r = "$r" ^ Int.toString r

  datatype regset = RS of int (* RS n represents { r | r >= n } *)

  fun smallest (RS n : regset) : reg = n
  fun -- ((RS n : regset), r : reg) : regset = if r >= n then RS (r + 1) else RS n

  infix 6 --
  (************ K-normalization ************)

  type exp = reg K.exp
  type policy = regset -> exp -> (reg -> exp) -> exp

  val emptyRS = RS 0
    (* puts the expression in an register, continues *)

  fun smartLet t e e' = (K.LET (t, e, e'))
  
  fun bindAnyReg  A (K.NAME n) k = k n
    | bindAnyReg A e k           = smartLet (smallest A) e (k (smallest A))
  
  fun bindSmallest A e k = smartLet (smallest A) e (k (smallest A))
                                    
  type 'a normalizer = regset -> 'a -> exp

  fun nbRegsWith normalize bind A [] k = k []
    | nbRegsWith normalize bind A (e::es) k = 
        let 
          val normalized = normalize A e 
          val bound      = bind A normalized (* (reg -> exp) -> exp *)
        in bound (fn r => nbRegsWith normalize bind (A -- r) es (fn rs => k (r::rs)))
        end

  val nbRegsWith : 'a normalizer -> policy -> regset -> 'a list -> (reg list -> exp) -> exp
    = nbRegsWith

  fun vLitCont (p, v) = (fn r => K.VMOP_LIT (p, [r],  v))
  
  fun removeRegisters reg_set [] = reg_set
    | removeRegisters reg_set (x::xs) = removeRegisters (reg_set -- x) xs

  (* val map':  ('a -> ('b -> 'answer) -> 'answer)
         -> 'a list
         -> ('b list -> 'answer)
         -> 'answer *)

  fun map' f' [] k = k []
    | map' f' (x :: xs) k =
      f' x (fn y => map' f' xs (fn ys => k (y :: ys)))
  
  fun exp rho A e =
    let val exp : reg Env.env -> regset -> ClosedScheme.exp -> exp = exp
        val nbRegs = nbRegsWith (exp rho)   (* normalize and bind in _this_ environment *)
        fun funcode (formals, e) = 
        let 
          val len = List.length formals
          val ns = List.tabulate (List.length formals, (fn x => x + 1))
        (* Create the register environment. *)
          val env = ListPair.foldlEq Env.bind Env.empty (formals, ns)
        in (ns, exp env (RS (len + 1)) e)
        end
        fun funcode' (nl, e) =  
        let
          val ls = List.tabulate (List.length nl, (fn x => x + 1))
          val rho' = ListPair.foldlEq (fn(n, l, rho) => E.bind (n, l, rho)) rho (nl, ls) 
          (* fun collate (n, (rho, A)) = (E.bind (n, smallest A, rho), A -- (smallest A))
          val (rho', A') = List.foldl collate (rho, emptyRS) ("zero"::nl) *)
          val A' = emptyRS -- (List.length ls)
          val body = exp rho' A' e
        in (ls, body, rho')
        end
    in  case e
          of C.LITERAL v => K.LIT v
           | C.PRIMCALL (p, es) => nbRegs bindAnyReg A es (fn rs => K.VMOP (p, rs))
           | C.LOCAL n => K.NAME (E.find (n, rho))
           | C.SETLOCAL (n, e) => K.ASSIGN (E.find(n, rho), exp rho A e)
           | C.GLOBAL n => KU.getglobal n
           | C.SETGLOBAL (n, e) => bindAnyReg A (exp rho A e) (fn r => KU.setglobal (n, r))
           | C.BEGIN [] => K.LIT (O.BOOL false) 
           | C.BEGIN es =>
            let fun decomposeBegin (e1::[]) = exp rho A e1
                  | decomposeBegin (e1::e2::[]) = K.SEQ(exp rho A e1, exp rho A e2)
                  | decomposeBegin (e1::es) = K.SEQ(exp rho A e1, exp rho A (C.BEGIN es))
                  | decomposeBegin _        = Impossible.impossible "Non top level empty begin"
             in decomposeBegin es
            end
           | C.FUNCALL (e, es) => 
            let fun k (r::rs) = K.FUNCALL(r, rs) 
                  | k _       = Impossible.impossible "Function must have a name"
            in nbRegs bindSmallest A (e::es) k
            end
           | C.IFX (e1, e2, e3) => bindAnyReg A (exp rho A e1) (fn r => K.IF_THEN_ELSE (r, exp rho A e2, exp rho A e3))
           | C.WHILEX (e, e') =>  K.WHILE (smallest A, (exp rho A e), (exp rho A e'))
           | C.LET (es, e') => 
            let 
              val (names,exps)   = ListPair.unzip es
              fun cont ls = 
                let
                  val (rho', A') = 
                  ListPair.foldl (fn (n, r, (env, set)) 
                                  => (Env.bind (n,r,env), set -- r)) (rho, A) (names, ls)
                in exp rho' A' e'
                end
            in nbRegs bindSmallest A exps cont 
            end
           | C.CLOSURE ((nl, e), []) => let val (ls, body, _) = funcode'(nl, e) in K.FUNCODE(ls, body) end
           | C.CLOSURE ((nl, e), captured) =>
            let
              val (ls, body) = funcode(nl, e)
            in nbRegs bindAnyReg A captured (fn rs => K.CLOSURE (ls, body, rs))
            end
           | C.CAPTURED i => K.CAPTURED i 
           | C.LETREC (bindings, body) => 
              let 
                fun fr len r = List.tabulate (len, (fn i => i + r))
                val (names, closures) = ListPair.unzip bindings
                val ts = fr (List.length bindings) (smallest A)
                val A' = removeRegisters A ts
                val rho' = ListPair.foldrEq (fn (n,r, rho) => Env.bind (n,r, rho)) rho (names, ts)
                fun closure (fc, captured) k = 
                      nbRegsWith (exp rho') bindAnyReg A' captured
                      (fn rs => k (funcode fc, rs))
              in
                map' (closure o snd) bindings (fn cs => let val cs' = map (fn ((i,j),n) => (i,j,n)) cs in K.LETREC (ListPair.zip (ts, cs'), exp rho' A' body) end)
              end
    end

 

  fun evalBindEmpty e k = bindAnyReg emptyRS (exp E.empty emptyRS e) k
  
  fun def (C.EXP e) = exp E.empty emptyRS e
    | def (C.CHECK_EXPECT (s1, e1, s2, e2)) = 
      let 
        val k1 = evalBindEmpty e1 (vLitCont (P.check, (O.STRING s1)))
        val k2 = evalBindEmpty e2 (vLitCont (P.expect, (O.STRING s2)))
      in K.SEQ (k1, k2)
      end
    | def (C.CHECK_ASSERT (n, e)) = evalBindEmpty e (vLitCont (P.check_assert, (O.STRING n)))
    | def (C.VAL (n, e)) = exp E.empty emptyRS (C.SETGLOBAL (n, e))
    | def (C.DEFINE (n, (nl, e))) = 
      let 
        fun collate (n, (rho, A)) = (E.bind (n, smallest A, rho), A -- (smallest A))
        val (rho, A) = List.foldl collate (Env.empty, emptyRS) (n::nl)
        val body = exp rho A e
        val ls = List.tabulate (List.length nl, (fn x => x + 1))
        in bindAnyReg A (K.FUNCODE (ls, body)) (fn r => KU.setglobal (n, r))
      end 
end
 
