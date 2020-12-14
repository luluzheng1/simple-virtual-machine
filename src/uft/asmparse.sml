(* A parser for assembly language *)


(* You'll get a partially complete version of this file, 
  which you'll need to complete. *)

structure AsmParse :>
  sig
    type line = string (* one line of assembly code *)
    val parse   : AsmLex.token list  list -> AssemblyCode.instr list Error.error
    val unparse : AssemblyCode.instr list -> line list
  end
  =
struct
  (* visualize list of tokens using Unicode middle dot as separator *)
  fun showTokens ts = "[" ^ String.concatWith "\194\183" (map AsmLex.unparse ts) ^ "]"

  structure P = MkListProducer (val species = "parser"
                                type input = AsmLex.token
                                val show = showTokens
                               )
      (* P for parser; builds a module that takes AsmLex.token as input *)

  structure L = AsmLex
  structure A = AssemblyCode
  structure O = ObjectCode

  type line = string (* one line of assembly code *)

  (* Operations on producers: Wishing for Modula-style FROM IMPORT here ... *)
  infix 3 <*>      val op <*> = P.<*>
  infixr 4 <$>     val op <$> = P.<$>
  infix 3 <~>      val op <~> = P.<~>
  infix 1 <|>      val op <|> = P.<|>
  infix 3 >>        val op >> = P.>>

  val succeed = P.succeed
  val curry = P.curry
  val curry3 = P.curry3
  val id = P.id
  val fst = P.fst
  val snd = P.snd
  val many = P.many
  val many1 = P.many1
  val sat = P.sat
  val one = P.one
  val notFollowedBy = P.notFollowedBy
  val eos = P.eos
  val fail = P.pzero
  fun flip f x y = f y x
  fun pair x y = (x, y)

  (* utilities *)

  fun eprint s = TextIO.output (TextIO.stdErr, s)

  type 'a parser = 'a P.producer

  (* always-error parser; useful for messages *)
  fun expected what =
    let fun bads ts = Error.ERROR ("looking for " ^ what ^
                                   ", got this input: " ^ showTokens ts)
    in  P.check ( bads <$> many one )
    end

  (* always-succeed parser; prints msg when run *)
  fun debug msg =
      P.ofFunction (fn ts => (app eprint ["@> ", msg, "\n"]; SOME (Error.OK (), ts)))

  (* make another parser chatter on entry *)
  val verbose : string -> 'a parser -> 'a parser
   = (fn msg => fn p => debug msg >> p)

  val veryVerbose : string -> 'a parser -> 'a parser
      = (fn what => fn p =>
           let fun shout s = app eprint ["looking for ", what, s, "\n"]
           in  P.ofFunction (fn ts =>
                                let val _ = shout "..."
                                    val answer = P.asFunction p ts
                                    val _ =
                                        case answer
                                          of NONE => shout ": failed"
                                           | SOME (Error.ERROR _, _) => shout ": errored"
                                           | SOME (Error.OK _, _) => shout ": succeeded"
                                in  answer
                                end)
           end)

  (****************************************************************************)

  (**** parsers for common tokens ****)

      (* These are your workhorse parsers---the analog of the `get`
         functions from the `tokens.h` interface in the SVM *)

  val int       = P.maybe (fn (L.INT   n)    => SOME n  | _ => NONE) one
  val name      = P.maybe (fn (L.NAME  n)    => SOME n  | _ => NONE) one
  val string    = P.maybe (fn (L.STRING s)   => SOME s  | _ => NONE) one
  val real      = P.maybe (fn (L.FPLIT n )  => SOME n  | _ => NONE) one
  val reg       = P.maybe (fn (L.REGISTER n) => SOME n  | _ => NONE) one
  val eol       = P.maybe (fn (L.EOL)        => SOME () | _ => NONE) one
  (* turn any single-token string into a parser for that token *)
  fun the "\n" = eol
    | the s =
        case AsmLex.tokenize s
          of Error.OK [t, AsmLex.EOL] => sat (P.eq t) one >> succeed ()
           | _ => Impossible.impossible "non-token in assembler parser"

  fun kw s = sat (P.eq s) name  (* keyword; example: kw "goto" *)


  val literal = O.INT <$> int <|> O.REAL <$> real <|> O.STRING <$> string 
                              <|> succeed (O.BOOL true) <~> the "true" 
                              <|> succeed (O.BOOL false) <~> the "false"

  (***** instruction-building functions for parsers ****)

  fun regs operator operands = A.OBJECT_CODE (O.REGS (operator, operands))
     (* curried instruction builder *)

  fun eR0 operator          = regs operator []
  fun eR1 operator r1       = regs operator [r1]
  fun eR2 operator r1 r2    = regs operator [r1, r2]
  fun eR3 operator r1 r2 r3 = regs operator [r1, r2, r3]

  fun eR1U16 operator r1 n = A.OBJECT_CODE (O.REGSLIT (operator, [r1], n))

  fun call r1 r2 (SOME r3) = A.OBJECT_CODE (O.REGS ("call", [r1,r2,r3]))
    | call r1 r2 (NONE)    = A.OBJECT_CODE (O.REGS ("call", [r1,r2,r2]))
  
  fun tailcall r1 (SOME r2) = A.OBJECT_CODE (O.REGS ("tailcall", [r1,r2]))
    | tailcall r1 (NONE)    = A.OBJECT_CODE (O.REGS ("tailcall", [r1,r1]))

  val binOps = ["cons", "eq",">", "<", "idiv", "+", "/", "*", "-", "set-car!"]
  val unOps  = ["function?", "symbol?", "pair?", "number?", "boolean?", "null?", "nil?", "car", "cdr", "hash"]
  val unSideEffectOps = ["error", "printu", "print", "println", "return"]
  (***** toy parser for you to extend ****)

  val parseHalt        = succeed (eR0 "halt") <~> the "halt"
  val parsePrint       = succeed (eR1 "print") <~> the "print" <*> reg
  val parseZero        = succeed (eR1 "zero") <~> the "zero" <*> reg
  val parseMov         = eR2 "mov" <$> reg <~> the ":=" <*> reg
  val parseBitwiseNot  = succeed (eR1 "~") <~> the "~" <*> reg 
  val parseBooleanNot  = succeed (eR1 "!") <~> the "!" <*> reg
  val parseLoadLit     = eR1U16 "loadliteral" <$> reg <~> the ":=" <*> literal
  val parseCheck       = succeed (eR1U16 "check") <~> the "check" <*> reg <*> O.STRING <$> string 
  val parseExpect      = succeed (eR1U16 "expect") <~> the "expect" <*> reg <*> O.STRING <$> string 
  val parseProjectBool = eR2 "projectbool" <$> reg <~> the ":=" <~> the "Bool" <~> the "(" <*> reg <~> the ")"
  val parseGetGlobal   = succeed (flip( eR1U16 "setglobal")) <~> the "globals" <~> the "[" <*> literal <~> the "]" <~> the ":=" <*> reg
  val parseSetGlobal   = succeed (eR1U16 "getglobal") <*> reg <~> the ":=" <~> the "globals" <~> the "[" <*> literal <~> the "]" 
  val parseDeflabel    = succeed A.DEFLABEL <~> the "def" <*> name
  val parseGotolabel   = succeed A.GOTO_LABEL <~> the "goto" <*> name
  val parseCallRegs    = succeed (call) <*> reg <~> the ":=" <~> the "call" <*> reg <~> the "(" <~> reg <~> the "," <~> the "..." <~> the "," <*> (P.optional reg) <~> the ")" 
  val parseCallReg     = succeed (call) <*> reg <~> the ":=" <~> the "call" <*> reg <~> the "(" <*> P.optional reg <~> the ")"
  val parseTailCallRs  = succeed (tailcall) <~> the "tailcall" <*> reg <~> the "(" <~> reg <~> the "," <~> the "..." <~> the "," <*> (P.optional reg) <~> the ")"  
  val parseTailCallR   = succeed (tailcall) <~> the "tailcall" <*> reg <~> the "(" <*> (P.optional reg) <~> the ")" 
  val parseMkClosure   = eR3 "mkclosure" <$> reg <~> the ":=" <~> the "closure" <~> the "[" <*> reg <~> the "," <*> reg <~> the "]"
  val parseGetClSlot   = eR3 "getclslot" <$> reg <~> the ":="  <*> reg <~> the "." <*> reg 
  val parseSetClSlot   = eR3 "setclslot" <$> reg <~> the "."  <*> reg <~> the ":=" <*> reg 
  (* val parseCallNoReg   = succeed (call) <*> reg <~> the ":=" <~> the "call" <*> reg <~> the "(" <~> the ")" *)
  
  
  fun unOpParser   (s, acc) = eR2 s <$> reg <~> the ":=" <~> the s <*> reg <|> acc
  fun unSideEffectParser (s, acc) = succeed (eR1 s) <~> the s <*> reg <|> acc
  fun binOpParsers (s, acc) = (eR3 s <$> reg <~> the ":=" <*> reg <~> kw s <*> reg) <|> acc
  (* parser to read an instruction /without/ reading end of line *)
  val one_line_instr : A.instr P.producer 
     =  kw "@" >> regs <$> name <*> many int  (* "escape hatch" syntax *)
    <|> List.foldl binOpParsers fail binOps
    <|> parseHalt 
    <|> parsePrint 
    <|> parseZero 
    <|> parseMov
    <|> parseBitwiseNot 
    <|> parseBooleanNot
    <|> parseLoadLit 
    <|> parseCheck 
    <|> parseExpect
    <|> parseProjectBool
    <|> parseGetGlobal
    <|> parseSetGlobal
    <|> parseDeflabel
    <|> parseGotolabel
    <|> parseCallRegs
    <|> parseCallReg
    <|> parseTailCallRs
    <|> parseTailCallR
    <|> parseMkClosure
    <|> parseGetClSlot
    <|> parseSetClSlot
    <|> List.foldl unOpParser fail unOps
    <|> List.foldl unSideEffectParser fail unSideEffectOps
    

  val if_goto_instr = the "if" >> curry A.IF_GOTO_LABEL <$> reg <~> many1 eol <~> the "goto" <*> name
   (**** recursive parser that handles end-of-line and function loading ****)

   (* Parsers for start and end of "load function", for you to write.
      Designing syntax so each one terminates with `eol` is recommended. *)

  fun loadfunc (reg, arity) body = A.LOADFUNC (reg, arity, body)
  val loadfunStart : (int * int) parser = (* fill in with (reg * arity) parser *)
        succeed pair <~> the "loadfunc" <*> reg <*> int <~> many eol <~> the "{" <~> many1 eol
  val loadfunEnd : unit parser =
        many eol >> the "}" <~> many1 eol

  fun badTokens ts = Error.ERROR ("unrecognized assembly line: " ^ showTokens ts)
  val nonEOL = sat (curry op <> L.EOL) one  (* any token except EOL *)

  val instruction : A.instr Error.error parser
    = P.fix (fn instruction =>
          Error.OK <$> if_goto_instr <~> many1 eol
      <|> Error.OK <$> one_line_instr <~> many1 eol
      <|> Error.OK <$>
          (loadfunc <$> loadfunStart <*> 
                        P.check (Error.list <$> many instruction) <~>
                        loadfunEnd)
      <|> P.notFollowedBy loadfunEnd >>
          (* gobble to end of line, then succeed by producing error message: *)
          badTokens <$> many nonEOL <~> eol  
      )

  val parse = 
    Error.join o 
    P.produce (Error.list <$> (many eol >> many instruction)) o 
    List.concat
            

  (*************************** unparsing *****************************)

  val int = Int.toString
  fun reg r = "r" ^ int r
  val spaceSep = String.concatWith " "

  fun toString (O.INT n) = int n
    | toString (O.STRING s) = "\"" ^ s ^ "\""
    | toString (O.REAL r) = Real.toString r
    | toString (O.BOOL b) = Bool.toString b
    | toString (O.EMPTYLIST) = "'()" 
    | toString _ = Impossible.impossible "Not a reconigzed literal value"

  (* For Regs *)
  fun unparseObjCode "+"           [x, y, z]        = spaceSep [reg x, ":=", reg y, "+", reg z] 
    | unparseObjCode "halt"        []               = "halt"
    | unparseObjCode "print"       [x]              = spaceSep ["print", reg x]
    | unparseObjCode "zero"        [x]              = spaceSep ["zero", reg x]
    | unparseObjCode "mov"         [x, y]           = spaceSep ["mov", reg x, reg y]
    | unparseObjCode "~"           [x]              = spaceSep ["~", reg x]
    | unparseObjCode "!"           [x]              = spaceSep ["!", reg x]
    | unparseObjCode "projectbool" [x, y]           = spaceSep [reg y, ":=", "Bool(" ^ reg x ^ ")"]
    | unparseObjCode "mkclosure"   [x, y, z]        = spaceSep [reg x, ":=", "closure[" ^ reg y, ",", Int.toString z ^ "]"]
    | unparseObjCode "getclslot"   [x, y, z]        = spaceSep [reg x, ":=", reg y ^ "." ^ Int.toString z]
    | unparseObjCode "setclslot"   [x, y, z]        = spaceSep [reg x ^ "." ^ Int.toString z, ":=", reg y]
    | unparseObjCode s             [x, y]           = spaceSep [reg x, ":=", s, reg y]
    | unparseObjCode s             [x, y, z]        = spaceSep [reg x, ":=", reg y, s, reg z] 
    | unparseObjCode s             [x] = spaceSep [s, reg x]
    | unparseObjCode _             _ = Impossible.impossible "unrecognized instruction"

  fun unparseObjCodeRLit "check" [x] (O.STRING s) = spaceSep ["check", reg x, "\"" ^ s ^ "\""]
    | unparseObjCodeRLit "expect" [x] (O.STRING s) = spaceSep ["expect", reg x, "\"" ^ s ^ "\""]
    | unparseObjCodeRLit "loadliteral" [x] lit = spaceSep [reg x, ":=", toString lit]
    | unparseObjCodeRLit "setglobal" [x] lit = spaceSep["globals" ^ "[" ^ toString lit ^ "]", ":=", reg x]
    | unparseObjCodeRLit "getglobal" [x] lit = spaceSep[reg x, ":=", "globals" ^ "[" ^ toString lit ^ "]"]
    | unparseObjCodeRLit _ _ _ = Impossible.impossible "unrecognized instruction"

  fun unparse1 (A.OBJECT_CODE (O.REGS (instr, reglist))) = unparseObjCode instr reglist
    | unparse1 (A.OBJECT_CODE (O.REGINT (instr, reg1, reg2, int))) = unparseObjCode instr [reg1, reg2, int]
    | unparse1 (A.OBJECT_CODE (O.REGSLIT (instr , reglist, lit))) = unparseObjCodeRLit instr reglist lit
    | unparse1 (A.DEFLABEL s) = spaceSep["def", s]
    | unparse1 (A.GOTO_LABEL s) = spaceSep["goto", s]
    | unparse1 (A.IF_GOTO_LABEL (x, s)) = spaceSep["if", reg x, "goto", s]
    | unparse1 _ = Impossible.impossible "an unknown assembly-code instruction"

  
  (* AssemblyCode.instr list -> string list *)
  fun unparse (x::xs) = (case x of 
                          A.LOADFUNC (x, arity, ilist) => [(spaceSep ["loadfunc", reg x, int arity, "{" ])] @ map (fn s => "  " ^ s) (unparse ilist) @ ["}"] @ (unparse xs)
                        | A.OBJECT_CODE (O.LOADFUNC (x, arity, ilist)) => [(spaceSep ["loadfunc", reg x, int arity, "{  "])] @ (unparse (map A.OBJECT_CODE ilist)) @ ["}"] @ (unparse xs)
                        | _ => unparse1 x :: unparse xs)
    | unparse []      = [] 



end


