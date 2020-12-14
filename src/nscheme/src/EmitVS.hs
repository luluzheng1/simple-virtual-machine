{-# LANGUAGE RecordWildCards #-}

module EmitVS where

import Control.Monad
import NScheme
import Prelude hiding (tail)

emitVS :: [NDef] -> IO ()
emitVS = mapM_ def

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = []

def :: NDef -> IO ()
def Val {n = x, e = e'} = do
  putStr $ "(val " <> x <> " "
  emitExp e'
  putStrLn ")"
def Def {n = name, lambda = l} = do
  putStr $ "(define " <> name <> " ("
  putStr $ if (head . formals) l == "()" then "" else (head . formals) l <> ")"
  emitExp $ LambdaE Lambda {formals = (tail . formals) l, lbody = lbody l}
  putStrLn ")"
def CheckE {..} = do
  putStr $ "(check-expect "
  emitExp check
  putStr " "
  emitExp expect
  putStrLn ")"
def Record {..} = error "Can't Emit Record Form in vScheme"

emitExp :: NExpr -> IO ()
emitExp Literal {val = v} = do
  literal v
emitExp Var {name = n} = putStr n
emitExp Prim {prim = n} = putStr n
emitExp IfX {cond = c, tCase = t, fCase = f} = do
  putStr "(if "
  emitExp c
  putStrLn " "
  emitExp t
  putStrLn " "
  emitExp f
  putStrLn ")"
emitExp WhileX {cond = c, body = b} = do
  putStr "(while "
  emitExp c
  putStr " "
  emitExp b
  putStrLn ")"
emitExp Begin {expList = es} = do
  putStr "(begin "
  mapM_ spaceBetween es
  putStrLn ")"
emitExp Apply {exp1 = e1, args = []} = emitExp e1
emitExp Apply {exp1 = e1, args = [a]} = do
  putStr "("
  emitExp e1
  putStr " "
  emitExp a
  putStr ")"
emitExp Apply {exp1 = Prim {prim = n}, args = [e1, e2]} = do
  putStr "("
  emitExp Prim {prim = n}
  putStr " "
  emitExp e1
  putStr " "
  emitExp e2
  putStr " )"
emitExp Apply {exp1 = e1, args = args} = do
  emitOpenParens args
  emitExp e1
  putStr " "
  emitCloseParens args
  where
    emitOpenParens args = mapM_ (\_ -> putChar '(') [0 .. length args - 1]
    emitCloseParens args = do
      mapM_ (\exp -> emitExp exp >> putStr ") ") args
emitExp (LetX LET formals body) = do
  putStr "(let* "
  putStr "("
  mapM_ putBinding formals
  putStr ")"
  putStr " "
  emitExp body
  putStr ")"
emitExp (LetX LETREC formals body) = do
  putStr "(letrec "
  putStr "("
  mapM_ putBinding formals
  putStr ")"
  putStr " "
  emitExp body
  putStr ")"
emitExp (LambdaE Lambda {formals = [], lbody = e}) = do
  emitExp e
emitExp (LambdaE Lambda {formals = f : fs, lbody = e}) = do
  putStr "(lambda ("
  if f /= "()" then putStr f else putStr ""
  putStr ") "
  emitExp (LambdaE Lambda {formals = fs, lbody = e})
  putStr ")"
emitExp (RecordE _ _) = putStr "Here!"

spaceBetween :: NExpr -> IO ()
spaceBetween v = do
  emitExp v
  putStr " "

putBinding :: NDef -> IO ()
putBinding Val {n = name, e = expr} = do
  putStr $ "[" <> name <> " "
  emitExp expr
  putStr "]"
putBinding Def {n = name, lambda = l} = do
  putStr $ "[" <> name <> " "
  emitExp (LambdaE l)
  putStr "]"

literal :: Value -> IO ()
literal (Sym s) = putChar '\'' >> putStr s
literal (Num n) = putStr $ show n
literal (Bool True) = putStr "#t"
literal (Bool False) = putStr "#f"
literal (Pair (v1, v2)) = do
  putStr "("
  literal v1
  putStr ","
  literal v2
  putStr ")"
literal Nil = putStr "'()"
literal (List v) =
  do
    putStr "'("
    let (first, last) = splitAt (length v - 1) v
     in if null last
          then do
            emitExp (head last)
            putStr ")"
          else do
            mapM_ printList first
            emitExp (head last)
            putStr ")"
  where
    printList e = do
      emitExp e
      putStr " "
