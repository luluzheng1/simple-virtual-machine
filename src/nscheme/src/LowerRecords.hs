{-# LANGUAGE RecordWildCards #-}

module LowerRecords (lowerRecords) where

import Data.List ((\\))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import GHC.IO (unsafePerformIO)
import NScheme

lowerRecords :: [NDef] -> ([NDef], [NDef])
lowerRecords nds =
  let records = filter extractRecords nds
      desugaredRecordFunctions = desugar records
      dds = desugaredRecordFunctions ++ (nds \\ records)
      noRecords = elimRecordForm (recordMap records) <$> dds
   in (records, noRecords)
  where
    extractRecords Record {..} = True
    extractRecords _ = False

elimRecordForm :: Map Name [Name] -> NDef -> NDef
elimRecordForm m = elimRecordForm'
  where
    elimRecordForm' Val {..} = Val {n = n, e = elimRecordExp m e}
    elimRecordForm' Def {..} = let newLambdaBody = elimRecordExp m $ lbody lambda in Def {n = n, lambda = Lambda {formals = formals lambda, lbody = newLambdaBody}}
    elimRecordForm' Record {..} = error "should be removed"
    elimRecordForm' CheckE {..} = CheckE {check = elimRecordExp m check, expect = elimRecordExp m expect}

elimRecordExp :: Map Name [Name] -> NExpr -> NExpr
elimRecordExp m e = elimRecordExp' e
  where
    elimRecordExp' (RecordE n (Right xs)) =
      let fields = m ! n
          args = toExpr (Map.fromList xs) <$> fields
       in Apply {exp1 = Var {name = "mk-" <> n}, args = elimRecordExp' <$> args}
    elimRecordExp' (RecordE n (Left (x, e))) = Apply {exp1 = Var {name = "get_" <> n <> "_" <> x}, args = [elimRecordExp' e]}
    elimRecordExp' (LambdaE l) = LambdaE (Lambda {formals = formals l, lbody = elimRecordExp' (lbody l)})
    elimRecordExp' IfX {..} = IfX {cond = elimRecordExp' cond, tCase = elimRecordExp' tCase, fCase = elimRecordExp' fCase}
    elimRecordExp' WhileX {..} = WhileX {cond = elimRecordExp' cond, body = elimRecordExp' body}
    elimRecordExp' (LetX lk ndfs expr) = LetX lk (elimRecordForm m <$> ndfs) (elimRecordExp' expr)
    elimRecordExp' Begin {..} = Begin {expList = elimRecordExp' <$> expList}
    elimRecordExp' Apply {..} = Apply {exp1 = elimRecordExp' exp1, args = elimRecordExp' <$> args}
    elimRecordExp' e = e

    toExpr args field = args ! field

desugar :: [NDef] -> [NDef]
desugar [] = []
desugar xs = genConstructors xs ++ genGetters xs

genSetters :: [NDef] -> [NDef]
genSetters = error "not implemented"

genGetters :: [NDef] -> [NDef]
genGetters xs = baseGetter : concat (baseCustomGetter <$> catMaybes (toTuple <$> xs))
  where
    toTuple Record {..} = Just (recName, fields)
    toTuple _ = Nothing

genConstructors :: [NDef] -> [NDef]
genConstructors xs = genConstructor <$> xs
  where
    genConstructor Record {..} =
      let (n, bindings) = foldl genBody ("", []) fields
          letbody = Var {name = "__tmp" <> n}
          lambda = Lambda {formals = fields, lbody = LetX LET (reverse bindings) letbody}
       in Def {n = "mk-" <> recName, lambda = lambda}

    genBody ("", cl) n = (n, Val {n = "__tmp" <> n, e = Apply {exp1 = Prim {prim = "cons"}, args = [Apply {exp1 = Prim {prim = "cons"}, args = [Literal {val = Sym n}, Var {name = n}]}, Literal {val = Nil}]}} : cl)
    genBody (e, cl) n = (n, Val {n = "__tmp" <> n, e = Apply {exp1 = Prim {prim = "cons"}, args = [Apply {exp1 = Prim {prim = "cons"}, args = [Literal {val = Sym n}, Var {name = n}]}, Var {name = "__tmp" <> e}]}} : cl)

recordMap :: [NDef] -> Map Name [Name]
recordMap xs = Map.fromList (toTuple <$> xs)
  where
    toTuple Record {..} = (recName, fields)
    toTuple _ = error "Fault in removing records"

baseCustomGetter :: (Name, [Name]) -> [NDef]
baseCustomGetter (x, fields) = concat $ perFieldGetter <$> fields
  where
    perFieldGetter f = [Val {n = "get_" <> x <> "_" <> f <> "?", e = LambdaE (Lambda {formals = [x], lbody = Apply {exp1 = Prim {prim = "=="}, args = [Apply {exp1 = Var {name = "car"}, args = [Var {name = x}]}, Literal {val = Sym f}]}})}, Val {n = "get_" <> x <> "_" <> f, e = Apply {exp1 = Var {name = "get_field"}, args = [Var {name = "get_" <> x <> "_" <> f <> "?"}]}}]

-- Below is not for human eyes

baseGetter :: NDef
baseGetter =
  Def
    { n = "get_field",
      lambda =
        Lambda
          { formals = ["p?", "xs"],
            lbody =
              Apply
                { exp1 =
                    IfX
                      { cond =
                          Apply {exp1 = Var {name = "atom?"}, args = [Var {name = "xs"}]},
                        tCase = Literal {val = Nil},
                        fCase = Apply {exp1 = IfX {cond = Apply {exp1 = Var {name = "null?"}, args = [Var {name = "xs"}]}, tCase = Apply {exp1 = Literal {val = Nil}, args = []}, fCase = Apply {exp1 = IfX {cond = Apply {exp1 = Apply {exp1 = Var {name = "p?"}, args = [Apply {exp1 = Var {name = "car"}, args = [Var {name = "xs"}]}]}, args = []}, tCase = Apply {exp1 = Apply {exp1 = Var {name = "cdr"}, args = [Apply {exp1 = Var {name = "car"}, args = [Var {name = "xs"}]}]}, args = []}, fCase = Apply {exp1 = Apply {exp1 = Var {name = "get_field"}, args = [Var {name = "p?"}, Apply {exp1 = Var {name = "cdr"}, args = [Var {name = "xs"}]}]}, args = []}}, args = []}}, args = []}
                      },
                  args = []
                }
          }
    }