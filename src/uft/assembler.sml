(* Label elimination: translate assembly code into virtual object code *)

(* You'll complete this file *)

structure Assembler :>
  sig
    val old_translate : AssemblyCode.instr list -> ObjectCode.instr list Error.error
      (* What can go wrong: An undefined or multiply-defined label *)
    val translate :AssemblyCode.instr list -> ObjectCode.instr list Error.error
  end
  =
struct
  structure A = AssemblyCode
  structure E = Env
  structure O = ObjectCode

  type 'a error = 'a Error.error
  val (succeed, <*>, <$>, >=>) = (Error.succeed, Error.<*>, Error.<$>, Error.>=>)
  infixr 4 <$>
  infix 3  <*>
  infix 2  >=>
  val fail = Error.ERROR

  fun curry f x y = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun flip f x y  = f y x
  fun cons x xs = x :: xs

  (* A "translation" that cannot handle any labels.  You will define a better one *)
  fun old_translate instrs =
    let fun cvt (A.OBJECT_CODE instr)       = Error.OK instr
          | cvt (A.LOADFUNC (r, k, instrs)) = curry3 O.LOADFUNC r k <$> old_translate instrs
          | cvt _                           = Error.ERROR "assembler not implemented"
    in  Error.list (map cvt instrs)
    end

  (* In lab, define `fold`, `lift`, `labelEnv`, `labelElim`, and `translate` here *)
  fun foldl f acc xs = 
    let fun ifold f i acc [] = acc
          | ifold f i acc (x::xs) =
              (case x of 
                  A.DEFLABEL _      => ifold f i (f x i acc) xs
                | A.IF_GOTO_LABEL _ => ifold f (i+2) (f x i acc) xs
                | _                 => ifold f (i+1) (f x i acc) xs)
    in ifold f 0 acc xs
    end

  fun foldr f acc xs =
    let fun ifold f i acc [] = acc
          | ifold f i acc (x::xs) =
              (case x of 
                  A.DEFLABEL _      => f(x, i, ifold f i acc xs)
                | A.IF_GOTO_LABEL _ => f(x, i, ifold f (i + 2) acc xs)
                | _                 => f(x, i, ifold f (i + 1) acc xs))
    in ifold f 0 acc xs
    end

  val lift : ('a * 'b * 'c -> 'c error) -> ('a * 'b * 'c error -> 'c error) 
    = (fn f => (fn (a, b, error) => case error of (Error.OK c)     => f(a, b, c)
                                                | (Error.ERROR s)    => error))

  fun duplicateLabelError label env = fail ("Label:" ^ label ^ " already defined on line" ^ Int.toString (E.find (label,env)))

  fun bindLabel ((A.DEFLABEL label), i, env) = if E.binds (env, label) then duplicateLabelError label env else Error.OK (E.bind (label, i, env))
    | bindLabel ( _,                 _, env) = Error.OK env

  fun labelEnv xs = foldl (curry3 (lift bindLabel)) (succeed E.empty) xs

 
  fun labelElim xs env =
    let fun labelHelper (A.LOADFUNC (r, n, xs), i, acc)    = curry op :: <$> ((curry3 O.LOADFUNC r n) <$> translate xs) <*> acc
          | labelHelper (A.OBJECT_CODE instr, i, acc)      = curry op :: <$> Error.OK instr <*> acc
          | labelHelper (A.DEFLABEL s, i, acc)             = acc
          | labelHelper (A.GOTO_LABEL s, i, acc)           = curry op :: <$> Error.OK (O.GOTO (Env.find (s, env) - i)) <*> acc
          | labelHelper (A.IF_GOTO_LABEL (reg, s), i, acc) = curry op @ <$> Error.OK [(O.REGS ("if", [reg])), (O.GOTO (Env.find(s, env) - i - 1))] <*> acc
    in foldr labelHelper (Error.OK []) xs
    end

  and translate xs = (labelEnv >=> labelElim xs) xs
  

  (* val translate :  AssemblyCode.instr list -> ObjectCode.instr list Error.error *)

end
