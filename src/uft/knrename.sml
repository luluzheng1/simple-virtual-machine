(* In K-normal form, convert string names to register names *)

(* You'll write this file *)

structure KNRename :> sig
  val regOfName : string -> ObjectCode.reg Error.error
  val mapx : ('a -> 'b Error.error) ->
             ('a KNormalForm.exp -> 'b KNormalForm.exp Error.error)
end
  =
struct
  structure K = KNormalForm
  structure P = Primitive

  infix 3 <*>   val op <*> = Error.<*>
  infixr 4 <$>  val op <$> = Error.<$>

  structure E = Error
  val succeed = Error.succeed
  val errorList = Error.list

  fun curry  f x y   = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun pair x y = (x, y)

  fun checky p = P.name p = "check" orelse P.name p = "expect"
  (* AsmLex.registerNum takes a string starting with "r" followed by a number n
     such that 0 <= n < 256, and returns n *)
  val regOfName = AsmLex.registerNum


  fun mapx f =
      let 
        fun translate (K.NAME n)                 = K.NAME <$> f n
          | translate (K.VMOP (p, xs))          = curry K.VMOP p <$> errorList (map f xs)
          | translate (K.VMOP_LIT (p, xs, l))   = curry3 K.VMOP_LIT p
                                                  <$> errorList (map f xs) 
                                                  <*> succeed l
          | translate (K.FUNCALL (funname, xs)) =    curry K.FUNCALL 
                                                  <$> f funname 
                                                  <*> errorList (map f xs)
          | translate (K.IF_THEN_ELSE (cond, e1, e2))      =    curry3 K.IF_THEN_ELSE 
                                                  <$> f cond 
                                                  <*> translate e1 
                                                  <*> translate e2
          | translate (K.LET (x, e, e'))        =     curry3 K.LET
                                                  <$> f x
                                                  <*> translate e
                                                  <*> translate e'
          | translate (K.SEQ (e1, e2))          =     curry K.SEQ 
                                                  <$> translate e1 
                                                  <*> translate e2
          | translate (K.ASSIGN (x, e))         =     curry K.ASSIGN
                                                  <$> f x
                                                  <*> translate e
          | translate (K.WHILE (x, e, e'))      =     curry3 K.WHILE
                                                  <$> f x
                                                  <*> translate e
                                                  <*> translate e'
          | translate (K.FUNCODE (xs, e))       =     curry K.FUNCODE
                                                <$> errorList (map f xs)
                                                  <*> translate e
          | translate (K.LIT l)             = succeed (K.LIT l)
          | translate (K.CAPTURED i)        = succeed (K.CAPTURED i)
          | translate (K.CLOSURE (xs, e, captured)) = curry3 K.CLOSURE
                                                                <$> errorList (map f xs)
                                                                <*> translate e
                                                                <*> errorList (map f captured)
          | translate (K.LETREC (ncls, e)) = 
          let fun create_letrec (n, (nl, e, ls)) = 
            let 
                fun lift n nl e ls = (n, (nl, e, ls))
            in
                lift <$> f n <*> errorList (map f nl) <*> translate e <*> errorList (map f ls)
            end
          in curry K.LETREC <$> errorList (map create_letrec ncls) <*> translate e
          end 
      in  translate
      end

      
end
