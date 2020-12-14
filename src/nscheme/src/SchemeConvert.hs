{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module SchemeConvert where

import Data.List ((\\))
import Data.Map hiding ((\\))
import qualified Data.Map as Map
import GHC.IO
import LowerRecords (lowerRecords)
import NScheme

desugar :: [NDef] -> [NDef]
desugar ds =
  let (records, dds) = lowerRecords ds
      requestedStlFunctions = getStl dds
      newStlFunctions = deSugarDef <$> getStlFunctions requestedStlFunctions
      ddds = newStlFunctions ++ (deSugarDef <$> dds)
   in ddds

desugar' :: [NDef] -> [NDef]
desugar' ds =
  let dds = (deSugarDef <$> ds)
   in dds

getStl :: [NDef] -> [Name]
getStl xs = concat $ getStlDef <$> xs

getStlDef :: NDef -> [Name]
getStlDef Val {..} = getStlExp e
getStlDef Def {..} = getStlExp $ lbody lambda
getStlDef CheckE {..} = getStlExp check ++ getStlExp expect

getStlExp :: NExpr -> [Name]
getStlExp Begin {..} = concat $ getStlExp <$> expList
getStlExp IfX {..} = getStlExp cond ++ getStlExp tCase ++ getStlExp fCase
getStlExp WhileX {..} = getStlExp cond ++ getStlExp body
getStlExp Apply {..} = getStlExp exp1 ++ concat (getStlExp <$> args)
getStlExp (LetX _ dl body) = getStl dl ++ getStlExp body
getStlExp (LambdaE Lambda {..}) = getStlExp lbody
getStlExp Var {..} = if name `elem` stlFunctions then [name] else []
getStlExp Prim {..} = if prim `elem` stlFunctions then [prim] else []
getStlExp _ = []

deSugarDef :: NDef -> NDef
deSugarDef Val {..} = Val {n = n, e = deSugarExp e}
deSugarDef Def {..} = Def {n = n, lambda = Lambda {formals = formals lambda, lbody = deSugarExp $ lbody lambda}}
deSugarDef CheckE {..} = CheckE {check = deSugarExp check, expect = deSugarExp expect}

deSugarExp :: NExpr -> NExpr
deSugarExp Begin {..} = Begin {expList = deSugarExp <$> expList}
deSugarExp IfX {..} = IfX {cond = deSugarExp cond, tCase = deSugarExp tCase, fCase = deSugarExp fCase}
deSugarExp WhileX {..} = WhileX {cond = deSugarExp cond, body = deSugarExp body}
deSugarExp Apply {..} = Apply {exp1 = deSugarExp exp1, args = deSugarExp <$> args}
deSugarExp (LetX lk dl body) = LetX lk (desugar' dl) (deSugarExp body)
deSugarExp (LambdaE Lambda {..}) = LambdaE (Lambda {formals = formals, lbody = deSugarExp lbody})
deSugarExp Prim {..}
  | Just p <- primMap !? prim = Prim {prim = p}
  | otherwise = Prim {..}
deSugarExp Var {..}
  | Just p <- primMap !? name = Var {name = p}
  | otherwise = Var {..}
deSugarExp e = e

-- hd -> car
primList :: [(Name, Name)]
primList =
  [ ("::", "cons"),
    ("hd", "car"),
    ("tl", "cdr"),
    ("==", "="),
    ("!", "deref"),
    (":=", "set-car!"),
    ("print_int", "print"),
    ("print_string", "print")
  ]

primMap :: Map Name Name
primMap = Map.fromList primList

stlFunctions :: [Name]
stlFunctions =
  [ "filter",
    "map",
    "foldr",
    "foldl",
    "reverse",
    "ref",
    "!",
    ":="
  ]

getStlFunctions :: [Name] -> [NDef]
getStlFunctions ns = concat [stlFunctionMap ! func | func <- ns, func `member` stlFunctionMap]

-- Below is not intended to be human readable
stlFunctionList :: [(Name, [NDef])]
stlFunctionList =
  [ ("filter", [Def {n = "filter", lambda = Lambda {formals = ["p", "xs"], lbody = Apply {exp1 = IfX {cond = Apply {exp1 = Var {name = "null?"}, args = [Var {name = "xs"}]}, tCase = Apply {exp1 = Literal {val = Nil}, args = []}, fCase = Apply {exp1 = IfX {cond = Apply {exp1 = Var {name = "p"}, args = [Apply {exp1 = Var {name = "car"}, args = [Var {name = "xs"}]}]}, tCase = Apply {exp1 = Prim {prim = "::"}, args = [Apply {exp1 = Var {name = "car"}, args = [Var {name = "xs"}]}, Apply {exp1 = Apply {exp1 = Var {name = "filter"}, args = [Var {name = "p"}, Apply {exp1 = Var {name = "cdr"}, args = [Var {name = "xs"}]}]}, args = []}]}, fCase = Apply {exp1 = Var {name = "filter"}, args = [Var {name = "p"}, Apply {exp1 = Var {name = "cdr"}, args = [Var {name = "xs"}]}]}}, args = []}}, args = []}}}]),
    ("map", [Def {n = "map", lambda = Lambda {formals = ["f", "xs"], lbody = Apply {exp1 = IfX {cond = Apply {exp1 = Var {name = "null?"}, args = [Var {name = "xs"}]}, tCase = Apply {exp1 = Literal {val = Nil}, args = []}, fCase = Apply {exp1 = Prim {prim = "::"}, args = [Apply {exp1 = Var {name = "f"}, args = [Apply {exp1 = Var {name = "hd"}, args = [Var {name = "xs"}]}]}, Apply {exp1 = Apply {exp1 = Var {name = "map"}, args = [Var {name = "f"}, Apply {exp1 = Var {name = "tl"}, args = [Var {name = "xs"}]}]}, args = []}]}}, args = []}}}]),
    ("reverse", [Def {n = "revapp", lambda = Lambda {formals = ["xs", "ys"], lbody = Apply {exp1 = IfX {cond = Apply {exp1 = Var {name = "null?"}, args = [Var {name = "xs"}]}, tCase = Apply {exp1 = Var {name = "ys"}, args = []}, fCase = Apply {exp1 = Var {name = "revapp"}, args = [Apply {exp1 = Var {name = "tl"}, args = [Var {name = "xs"}]}, Apply {exp1 = Prim {prim = "::"}, args = [Apply {exp1 = Var {name = "hd"}, args = [Var {name = "xs"}]}, Var {name = "ys"}]}]}}, args = []}}}, Def {n = "reverse", lambda = Lambda {formals = ["xs"], lbody = Apply {exp1 = Var {name = "revapp"}, args = [Var {name = "xs"}, Literal {val = Nil}]}}}]),
    ("ref", [Def {n = "ref", lambda = Lambda {formals = ["x"], lbody = Apply {exp1 = Prim {prim = "::"}, args = [Var {name = "x"}, Literal {val = Nil}]}}}]),
    ("!", [Def {n = "deref", lambda = Lambda {formals = ["x"], lbody = Apply {exp1 = Var {name = "hd"}, args = [Var {name = "x"}]}}}])
  ]

stlFunctionMap :: Map Name [NDef]
stlFunctionMap = Map.fromList stlFunctionList