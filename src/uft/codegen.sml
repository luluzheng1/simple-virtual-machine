(* Generates abstract assembly code from KNormal-form VScheme *)

(* You'll write this file *)

structure Codegen
  :>
sig 
  type reg = ObjectCode.reg
  type instruction = AssemblyCode.instr
  val forEffect : reg KNormalForm.exp -> instruction list
end
  =
struct
  structure A = AsmGen
  structure K = KNormalForm
  structure P = Primitive

  type reg = ObjectCode.reg
  type instruction = AssemblyCode.instr

  (********* Join lists, John Hughes (1986) style *********)

  type 'a hughes_list = 'a list -> 'a list
    (* append these lists using `o` *)

  (* don't look at these implementations; look at the types below! *)
  fun empty tail = tail
  fun S e  tail = e :: tail
  fun L es tail = es @ tail
  val _ = empty : 'a hughes_list
  val _ = S     : 'a      -> 'a hughes_list   (* singleton *)
  val _ = L     : 'a list -> 'a hughes_list   (* conversion *)

  val newlabel = A.newlabel
  (* val goto = A.goto (newlabel ()) *)

  fun const f = (fn x => fn y => f x)
  fun flip f = (fn x => fn y => f y x)
  
  val hconcat : 'a hughes_list list -> 'a hughes_list
    = fn xs => foldr op o empty xs

(* mapi : (int * 'a ‑> 'b) ‑> 'a list ‑> 'b list *)
  fun mapi f xs =  (* missing from mosml *)
    let fun go k [] = []
          | go k (x::xs) = f (k, x) :: go (k + 1) xs
    in  go 0 xs
  end

(************** the code generator ******************)

  (* three contexts for code generation: to put into a register,
     to run for side effect, or (in module 8) to return. *)
  fun letrec gen (bindings, body) =
   let val _ = letrec : (reg K.exp -> instruction hughes_list)
                     -> (reg * reg K.closure) list * reg K.exp
                     -> instruction hughes_list
      (* one helper function to allocate and another to initialize *)
      fun alloc (f_i, (formals, body, captures)) = toReg' f_i (K.FUNCODE (formals,body)) o S (A.mkclosure f_i f_i ((List.length formals) +1))
      fun init  (f_i, (formals, body, captures)) = create_slots f_i captures
  in  hconcat (map alloc bindings) o hconcat (map init bindings) o gen body
  end
  and toReg' (dest : reg) (e : reg KNormalForm.exp) : instruction hughes_list =
        (case e of
          (K.LIT v) => S (A.loadlit dest v)
          | (K.NAME x) => S (A.copyreg dest x)
          | (K.VMOP ((P.SETS_REGISTER a), xs)) => S (A.setreg dest (P.SETS_REGISTER a) xs)
          | (K.VMOP ((P.HAS_EFFECT a), xs)) => S (A.effect (P.HAS_EFFECT a) xs)
          | (K.VMOP_LIT ((P.SETS_REGISTER a), xs, v)) => S (A.setregLit dest (P.SETS_REGISTER a) xs v)
          | (e as (K.VMOP_LIT ((P.HAS_EFFECT a), xs, v))) => forEffect' e
          | (K.IF_THEN_ELSE (x, e, e')) => if_else_helper (x, e, e', (flip toReg'), dest)
          | (K.FUNCALL (x, xs)) => if A.areConsecutive xs then S (A.call dest x (case last xs of 
                                                                                    NONE => x
                                                                       | SOME l => l))  else Impossible.impossible "Registers need to be consecutive"
          | (K.LET (x, e, e')) =>  (toReg' x e) o (toReg' dest e')
          | (K.SEQ (e1, e2)) => (forEffect' e1) o (toReg' dest e2)
          | (K.ASSIGN (x, e)) => (toReg' x e) o S (A.copyreg dest x)
          | (K.WHILE (x, e, e')) => (forEffect' (K.WHILE (x, e, e'))) o S (A.loadlit dest (ObjectCode.BOOL false))
          | (K.FUNCODE (xs, e)) => S (A.loadfunc dest (List.length xs) (return e []))
          | (K.CLOSURE (f, e, c)) => S (A.loadfunc dest (List.length f) (return e [])) o 
                                    S (A.mkclosure dest dest (List.length c)) o 
                                    create_slots dest c
          | (K.CAPTURED i) =>  S (A.captured dest i)
          | (K.LETREC (bindings, body)) => letrec (toReg' dest) (bindings, body)
          | _ => empty)
  and forEffect' (e: reg KNormalForm.exp) : instruction hughes_list  =
        (case e of 
        (K.LET (x,e,e')) => (toReg' x e) o (forEffect' e')
        | (K.VMOP ((P.HAS_EFFECT a), xs)) => S (A.effect (P.HAS_EFFECT a) xs)
        | (K.IF_THEN_ELSE (x, e, e')) => if_else_helper (x, e, e', const forEffect', 0) 
        | (e' as (K.FUNCALL (x,xs)))  => toReg' x e'
        | (K.VMOP_LIT ((P.HAS_EFFECT a), xs, v)) =>  S (A.effectLit (P.HAS_EFFECT a) xs v)
        | (K.ASSIGN (x, e')) => toReg' x e'
        | (K.SEQ (e1, e2)) => (forEffect' e1) o (forEffect' e2)
        | (K.WHILE (x, e1, e2)) => while_effect x e1 e2
        | (K.LETREC (bindings, body)) => letrec forEffect' (bindings, body)
        | _ => empty)

  and return (K.FUNCALL (x,xs)) = if A.areConsecutive xs then S (A.tailcall x (case last xs of 
                                                    NONE => x
                                                  | SOME l => l)) else Impossible.impossible "Registers need to be consecutive"
    | return (K.NAME x) = S (A.return x)
    | return (e as K.LIT v) = toReg' 0 e o S (A.return 0)
    | return (K.IF_THEN_ELSE (x, e, e')) = if_else_helper (x, e, e', const return, 0)
    | return (K.LET (x, e, e')) = toReg' x e o return e'
    | return (K.SEQ (e1, e2)) = forEffect' e1 o return e2
    | return (e as K.VMOP (vmop, xs)) = toReg' 0 e o S (A.return 0) 
    | return (e as K.VMOP_LIT (vmop, xs, v)) = toReg' 0 e o S (A.return 0)
    | return (K.CAPTURED i) = S (A.getclslot 0 0 i) o S (A.return 0)
    | return (e as K.CLOSURE (xs, e', cl)) = toReg' 0 e o S (A.return 0)
    | return (K.LETREC (bindings, body)) = letrec return (bindings, body)
    | return (e as K.WHILE (x, e1, e2)) = toReg' 0 e o S (A.return 0)
    | return (K.ASSIGN (x, e)) = toReg' x e o S (A.return x)
    | return _ = Impossible.impossible "it's just impossible"
  and last [] = NONE
    | last (x::xs) = SOME (last' x xs)
  and last' x []      = x 
    | last' _ (x::xs) = last' x xs
  and if_else_helper (x, e, e', f, r) = let 
                          val L = newlabel ()
                          val L' = newlabel ()
                      in (S (A.ifgoto x L)) o 
                        (f e' r) o 
                        (S (A.goto L')) o 
                        (S (A.deflabel L)) o 
                        (f e r) o 
                        (S (A.deflabel L')) 
                      end
  and while_effect x e e' = let 
                          val L = newlabel ()
                          val L' = newlabel ()
                      in (S (A.goto L)) o 
                      (S (A.deflabel L')) o 
                      (forEffect' e') o 
                      (S (A.deflabel L)) o 
                      (toReg' x e) o 
                      (S (A.ifgoto x L'))
                      end 
  and create_slots dest cl = hconcat (mapi (fn (i, c) => S (A.setclslot dest i c)) cl)

  val _ = forEffect' :        reg KNormalForm.exp -> instruction hughes_list
  val _ = toReg'     : reg -> reg KNormalForm.exp -> instruction hughes_list

  fun forEffect e = forEffect' e []


end
