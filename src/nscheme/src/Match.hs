{-# LANGUAGE RecordWildCards #-}

module Match where

import NScheme

unrollMatches :: [NDef] -> [NDef]
unrollMatches xs = unrollMatchDef <$> xs

unrollMatchDef :: NDef -> NDef
unrollMatchDef Record {..} = Record {..}
unrollMatchDef CheckE {..} = CheckE {check = unrollMatchExp check, expect = unrollMatchExp expect}
unrollMatchDef Val {..} = Val {n = n, e = unrollMatchExp e}
unrollMatchDef Def {..} = Def {n = n, lambda = Lambda {formals = formals lambda, lbody = unrollMatchExp $ lbody lambda}}

unrollMatchExp :: NExpr -> NExpr
unrollMatchExp IfX {..} = IfX {cond = unrollMatchExp cond, tCase = unrollMatchExp tCase, fCase = unrollMatchExp fCase}
unrollMatchExp WhileX {..} = WhileX {cond = unrollMatchExp cond, body = unrollMatchExp body}
unrollMatchExp Begin {..} = Begin {expList = unrollMatchExp <$> expList}
unrollMatchExp Apply {..} = Apply {exp1 = unrollMatchExp exp1, args = unrollMatchExp <$> args}
unrollMatchExp (LetX lk xs e) = LetX lk (unrollMatches xs) (unrollMatchExp e)
unrollMatchExp (LambdaE (Lambda {..})) = LambdaE (Lambda {formals = formals, lbody = unrollMatchExp lbody})
unrollMatchExp (Match n xs) = unrollMatch n xs
unrollMatchExp e = e

unrollMatch :: Name -> [(Guard, NExpr)] -> NExpr
unrollMatch _ [] = Apply {exp1 = Var {name = "error"}, args = [Literal {val = Sym "Failed_Pattern_Match"}]}
unrollMatch n ((GuardVar g, e) : _) = LetX LET [Val {n = g, e = Var {name = n}}] e
unrollMatch n (((GuardRec recName rs), e) : xs) =
  let (vars, vals) = extractGuards rs
      cond = genRecordCond n recName vals
      tCase = genMatchBranch n recName vars e
   in IfX {cond = cond, tCase = tCase, fCase = unrollMatch n xs}
unrollMatch n ((g, e) : xs) = IfX {cond = genCond n g, tCase = unrollMatchExp e, fCase = unrollMatch n xs}

genMatchBranch :: Name -> Name -> [(Name, Guard)] -> NExpr -> NExpr
genMatchBranch var recName vars e =
  let bindings = map (\(getter, GuardVar v) -> Val {n = v, e = getFromRecord var recName getter}) vars
   in LetX LET bindings $ unrollMatchExp e

genRecordCond :: Name -> Name -> [(Name, Guard)] -> NExpr
genRecordCond _ _ [] = Literal {val = Bool True}
genRecordCond var recName [(n, GuardVal v)] = Apply {exp1 = Prim {prim = "="}, args = [getFromRecord var recName n, Literal {val = v}]}
genRecordCond var recName ((n, GuardVal v) : xs) = Apply {exp1 = Prim {prim = "&&"}, args = [Apply {exp1 = Prim {prim = "="}, args = [getFromRecord var recName n, Literal {val = v}]}, genRecordCond var recName xs]}

genCond :: Name -> Guard -> NExpr
genCond n (GuardVal g) = Apply {exp1 = Prim {prim = "="}, args = [Var {name = n}, Literal {val = g}]}
genCond _ _ = undefined

extractGuards :: [(Name, Guard)] -> ([(Name, Guard)], [(Name, Guard)])
extractGuards xs =
  let vars = filter filterVar xs
      vals = filter filterVal xs
   in (vars, vals)

filterVal :: (Name, Guard) -> Bool
filterVal (_, (GuardVal _)) = True
filterVal _ = False

filterVar :: (Name, Guard) -> Bool
filterVar (_, GuardVar _) = True
filterVar _ = False

getFromRecord :: Name -> Name -> Name -> NExpr
getFromRecord var recName fieldName = RecordE recName (Left (fieldName, Var {name = var}))