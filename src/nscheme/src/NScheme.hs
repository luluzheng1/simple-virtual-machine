{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module NScheme where

data Value = Sym String | Num Int | Bool Bool | Pair (Value, Value) | Nil | List [NExpr] deriving (Eq, Show)

data Guard = GuardVal Value | GuardVar Name | GuardRec Name [(Name, Guard)] deriving (Eq, Show)

data NExpr
  = Literal {val :: Value}
  | Var {name :: Name}
  | Prim {prim :: Name}
  | IfX {cond :: NExpr, tCase :: NExpr, fCase :: NExpr}
  | WhileX {cond :: NExpr, body :: NExpr}
  | Begin {expList :: [NExpr]}
  | Apply {exp1 :: NExpr, args :: [NExpr]}
  | LetX LetKind [NDef] (NExpr)
  | LambdaE (Lambda)
  | Match Name [(Guard, NExpr)]
  | RecordE Name (Either (Name, NExpr) [(Name, NExpr)])
  deriving (Eq, Show) 

data LetKind = LET | LETREC deriving (Eq, Show)

type Name = String

data Lambda = Lambda {formals :: [Name], lbody :: NExpr} deriving (Eq, Show)

data NDef
  = Val {n :: Name, e :: NExpr}
  | Def {n :: Name, lambda :: Lambda}
  | CheckE {check :: NExpr, expect :: NExpr}
  | Record {recName :: Name, fields :: [Name]}
  deriving (Eq, Show)
