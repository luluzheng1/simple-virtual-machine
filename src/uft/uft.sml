(* This is the universal forward translator. As you build the different VScheme 
    representations and the translations between them, you'll chain together 
    these translations here. It implements the actual translations *)

(* You'll get a partially complete version of this file, 
    which you'll need to complete *)

structure UFT :> sig
  type language = Languages.language
  exception NotForward of language * language
  val translate : language * language -> TextIO.instream * TextIO.outstream -> unit Error.error
  exception NoTranslationTo of language
end
  =
struct

  (**** I/O functions and types ****)

  type instream = TextIO.instream
  val lines  = IOUtil.lines : instream -> string list
  val output = IOUtil.output 
  val outln  = IOUtil.outln
  val execute = Unix.execute


  (**** function composition, including errors ****)

  type 'a error = 'a Error.error

  infix 0 >>> >=>
  fun f >>> g = fn x => g (f x)         (* function composition, Elm style *)
  val op >=> = Error.>=>

  fun liftMap f xs = Error.list (map f xs)  (* liftMap f == map f >>> Error.list *)



  (**** Reader functions ****)

  val schemeOfFile : instream -> VScheme.def list error =
    lines                                   (* line list *)
    >>>  SxParse.parse                      (* sx list error *)
    >=>  Error.mapList VSchemeParsers.defs  (* def list list error *)
    >>>  Error.map List.concat              (* def list error *)
    >>>  Error.map VSchemeTests.delay
    
  val schemexOfFile : instream -> UnambiguousVScheme.def list error =
    schemeOfFile >>>
    Error.map (map Disambiguate.disambiguate)

  val VS_of_file : instream -> AssemblyCode.instr list error =
    lines                    (* line list *)
    >>> map AsmLex.tokenize  (* token list error list *)
    >>> Error.list           (* token list list error *)
    >=> AsmParse.parse       (* instr list error *)    

  val FO_of_file : instream -> FirstOrderScheme.def list error =
    schemexOfFile >=> liftMap FOUtil.project 
  val KN_of_file : instream -> string KNormalForm.exp list error =
    schemexOfFile >=> liftMap ProjectKN.def 


  (**** Materializer functions ****)
  
  exception Backward  (* for internal use only *)
                      (* raised if someone commands a backward translation *)

  datatype language = datatype Languages.language (* imports value constructors *)
  exception NoTranslationTo of language  (* used externally *)

  val ! = Error.map  (* useful abbreviation for materializers and `translate` *)

  fun FO_of FO   = FO_of_file
    | FO_of inLang = raise Backward  
  
  fun HOX_of HOX  = schemexOfFile
    | HOX_of _    = raise Backward

  fun HO_of HOX  = schemexOfFile >>> ! (map Mutability.moveToHeap)
    | HO_of HO   = schemexOfFile >=> Error.mapList Mutability.detect
    | HO_of NS   = raise NoTranslationTo NS
    | HO_of _    = raise Backward

  fun CL_of CL     = CL_of FO   (* really *)
    | CL_of HO     = HO_of HO     >>> ! (map ClosureConvert.close)
    | CL_of HOX    = HO_of HOX    >>> ! (map ClosureConvert.close)
    | CL_of NS = HO_of NS >>> ! (map ClosureConvert.close)
    | CL_of _ = raise Backward
    (* FO_of inLang >>> ! (map FOCLUtil.embed) *)

  fun KN_reg_of KN = KN_of_file (* string exp list error *) 
                      >=> (Error.mapList (KNRename.mapx KNRename.regOfName)) (* reg exp list error *)    
    | KN_reg_of inLang = CL_of inLang  >>> ! (List.map KNormalize.def)
  fun VS_of VS   = VS_of_file
    | VS_of inLang = KN_reg_of inLang >>> ! 
                     (map Codegen.forEffect) >>> !  (* instr list list error*)
                     List.concat (* instruction list error *)
  val _ = VS_of : language -> instream -> AssemblyCode.instr list error
  fun VO_of VO     = (fn _ => Error.ERROR "There is no reader for .vo")
    | VO_of inLang = VS_of inLang >=> Assembler.translate

  fun KN_text_of KN   = KN_of_file
    | KN_text_of inLang = KN_reg_of inLang >=>
                          Error.mapList (KNRename.mapx (Error.OK o KNormalize.regname))





  (**** Emitter functions ****)

  val width =  (* parameter to prettyprinter *)
    case Option.mapPartial Int.fromString (OS.Process.getEnv "WIDTH")
      of SOME n => n
       | NONE => 72

  fun emitVO outfile = app (outln outfile) o ObjectUnparser.module
  fun emitVS outfile = app (outln outfile) o AsmParse.unparse

  fun emitScheme outfile = Wppx.toOutStream width outfile o WppScheme.pp

  fun emitHO outfile = app (emitScheme outfile o Disambiguate.ambiguate)

  fun emitKN outfile = app (emitScheme outfile o EmbedKN.def)
  
  fun emitFO outfile = app (emitScheme outfile o FOUtil.embed)

  fun emitCL outfile = app (emitScheme outfile o CSUtil.embed)
  

  (**** The Universal Forward Translator ****)

  exception NotForward of language * language  (* for external consumption *)

  fun translate (inLang, outLang) (infile, outfile) =
    (case outLang
       of VO => VO_of      inLang >>> ! (emitVO outfile)
        | VS => VS_of      inLang >>> ! (emitVS outfile)
        | CL => CL_of      inLang >>> ! (emitCL outfile)
        | HO => HO_of      inLang >>> ! (emitHO outfile)
        | HOX => HOX_of    inLang >>> ! (emitHO outfile)
        | FO => FO_of      inLang >>> ! (emitFO outfile)
        | KN => KN_text_of inLang >>> ! (emitKN outfile)
        | _ => raise Backward 
    ) infile
    handle Backward => raise NotForward (inLang, outLang)
         | NoTranslationTo outLang => raise NotForward (inLang, outLang)
end
