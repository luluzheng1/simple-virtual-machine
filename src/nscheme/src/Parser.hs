{-# LANGUAGE DoAndIfThenElse #-}

module Parser where

import Data.Char (isDigit, isLetter)
import Data.Functor.Identity
import Data.Semigroup ()
-- import Text.Parsec (ParsecT, anyChar, between, char, choice, digit, eof, lookAhead, many, many1, manyTill, newline, noneOf, oneOf, option, optional, satisfy, sepBy, skipMany, spaces, string, try, (<?>), (<|>))

import Data.Void (Void)
import NScheme
  ( Guard (GuardRec, GuardVal, GuardVar),
    Lambda (Lambda, formals, lbody),
    LetKind (LET, LETREC),
    NDef (..),
    NExpr (..),
    Name,
    Value (..),
  )
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer (symbol)
import Text.Megaparsec.Char

type Parser a = Parsec Void String a

reserved :: [Name]
reserved = ["do", "true", "false", "while", "let", "if", "then", "else", "in", "done", "begin", "end", "fun", "match", "with"]

binopPrim :: [Name]
binopPrim = ["<=", ">=", "<", ">", "==", "!=", "+", "-", "/", "*", "::", "mod", "&&", "||", ":=", "and"]

unopPrim :: [Name]
unopPrim = ["null?", "ref", "!", "print_string", "print_int", "print"]

isReserved :: Name -> Bool
isReserved s = s `elem` reserved

newSpaces :: Parser ()
newSpaces = do
  skipMany (oneOf " \t")

parseValue :: Parser Value
parseValue = try parseNil <|> try parseList <|> try parseTuple <|> try parseInt <|> try parseBool <|> try parseSym
  where
    parseTuple = do
      newSpaces
      char '('
      space
      v1 <- parseValue
      space
      char ','
      space
      v2 <- parseValue
      space
      char ')'
      return $ Pair (v1, v2)
    parseInt = do
      newSpaces
      v <- some digitChar
      return $ Num (read v)
    parseBool = do
      newSpaces
      b <- string "true" <|> string "false"
      return $ Bool (b == "true")
    parseSym = do
      newSpaces
      Sym <$> parseString
    parseNil = do
      newSpaces
      char '['
      space
      char ']'
      return Nil
    parseList = do
      newSpaces
      l <- between (char '[') (char ']') ((space >> parseExpAtomic <* space) `sepBy` char ';')
      return $ List l

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser String
parseString = do
  char '"'
  strings <- many character
  char '"'
  return $ concat strings

parseName :: Parser Name
parseName = try parseRegularName <|> parseUnit
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_' || a == '?')
    parseRegularName = do
      fc <- firstChar
      rest <- many nonFirstChar
      if isReserved (fc : rest)
        then fail "reserved word"
        else return (fc : rest)
    parseUnit = do
      char '('
      newSpaces
      char ')'
      return "()"

parseBinOp :: Parser Name
parseBinOp = choice $ try <$> string <$> binopPrim

parseUnOp :: Parser Name
parseUnOp = choice $ try <$> string <$> unopPrim

parseExp :: Parser NExpr
parseExp = choice [try parseInfixOp, try parseApply, try parseLambda, try parseExpAtomic]
  where
    parseInfixOp = do
      newSpaces
      e1 <- try parseExpAtomic
      newSpaces
      op <- parseBinOp
      if op `notElem` binopPrim
        then fail "Not a binary operator"
        else do
          newSpaces
          e2 <- try parseExp
          return $ Apply {exp1 = Prim {prim = op}, args = [e1, e2]}
    parseApply = do
      (x : xs) <- some (try (newSpaces >> parseExpAtomic))
      return Apply {exp1 = x, args = xs}
    parseLambda = do
      newSpaces
      string "fun" <|> string "function"
      newSpaces
      formals <- some parseFormals
      newSpaces
      string "->"
      newSpaces
      e <- parseExp
      return $ LambdaE Lambda {formals = formals, lbody = e}
    parseFormals = do
      formal <- parseName
      newSpaces
      return formal

parseExpAtomic :: Parser NExpr
parseExpAtomic = try parseParenExp <|> try parseMakeRecord <|> try parseGetRecord <|> try parseMatch <|> try parseLet <|> try parseBegin <|> try parseWhile <|> try parseIf <|> try parseLit <|> try parseVar <|> try parseUnaryPrim
  where
    parseLit = do
      v <- parseValue
      return $ Literal {val = v}
    parseUnaryPrim = do
      prim <- parseUnOp
      return Prim {prim = prim}
    parseVar = do
      n <- parseName
      return Var {name = n}
    parseIf = do
      string "if"
      newSpaces
      e1 <- parseExp
      space
      string "then"
      newSpaces
      e2 <- parseExp
      space
      string "else"
      newSpaces
      e3 <- parseExp
      return IfX {cond = e1, tCase = e2, fCase = e3}
    parseParenExp = do
      newSpaces
      between (char '(') (char ')') parseExp

    parseWhile = do
      string "while"
      newSpaces
      e1 <- parseExp
      newSpaces
      e2 <- parseDo
      return WhileX {cond = e1, body = e2}

    parseDo = do
      string "do"
      es <- some (try (space >> parseExp))
      space
      string "done"
      return Begin {expList = es}

    parseBegin = do
      string "begin"
      es <- some (try (space >> parseExp))
      space
      string "end"
      return Begin {expList = es}
    parseLet = do
      newSpaces
      string "let"
      newSpaces
      letkind <- option "" (string "rec")
      bindings <- many (try parseVal)
      space
      string "in"
      newSpaces
      LetX (if letkind /= "" then LETREC else LET) bindings <$> parseExp
    parseMakeRecord = do
      newSpaces
      char '#'
      n <- parseName
      newSpaces
      a <- between (char '{') (char '}') (many (do newSpaces; n <- parseName; newSpaces; char '='; newSpaces; e <- parseExpAtomic; newSpaces; char ';'; return (n, e)))
      return $ RecordE n (Right a)
    parseGetRecord = do
      newSpaces
      char '#'
      n <- parseName
      char '.'
      n1 <- parseName
      newSpaces
      e <- parseExpAtomic
      return $ RecordE n (Left (n1, e))
    parseMatch = do
      newSpaces
      string "match"
      newSpaces
      n <- parseName -- Value we are matching
      newSpaces
      string "with"
      matches <- some (try parseGuardExp)
      return $ Match n matches
      where
        parseGuardExp = do
          space
          char '|'
          newSpaces
          g <- (try parseGuardRecord <|> try parseGuardVal <|> try parseGuardVar)
          newSpaces
          string "->"
          newSpaces
          e <- parseExpAtomic
          return (g, e)
        parseGuardVal = do
          newSpaces
          GuardVal <$> parseValue
        parseGuardVar = do
          newSpaces
          GuardVar <$> parseName
        parseGuardRecord = do
          n <- parseName
          newSpaces
          a <- between (char '{') (char '}') (some (do newSpaces; n <- parseName; newSpaces; char '='; newSpaces; e <- try parseGuardVal <|> try parseGuardVar; newSpaces; char ';'; return (n, e)))
          return (GuardRec n a)

parseNDef :: Parser [NDef]
parseNDef = manyTill ((try (space *> string "let" *> parseVal) <|> parseDef <|> parseCheckE <|> parseRecord) <?> "Could not parse val or def") eof
  where
    parseDef =
      do
        space
        string "let"
        space
        funName <- parseName <?> "Failed at parsing a name"
        space
        formals <- some parseFormals
        space
        char '='
        space
        exp <- parseExp
        space
        return Def {n = funName, lambda = Lambda {formals = formals, lbody = exp}}
    parseFormals = do
      formal <- parseName
      space
      return formal
    parseCheckE = do
      space
      string "check-expect"
      newSpaces
      e1 <- parseExpAtomic
      newSpaces
      e2 <- parseExpAtomic
      space
      return CheckE {check = e1, expect = e2}
    parseRecord = do
      space
      string "type"
      newSpaces
      n <- parseName
      newSpaces
      string "="
      space
      fields <- between (char '{') (char '}') (some (newSpaces *> parseName <* newSpaces <* char ';' <* space))
      space
      return Record {recName = n, fields = fields}

parseVal :: Parser NDef
parseVal =
  do
    space
    name <- parseName <?> "Failed at parsing a name"
    newSpaces
    char '='
    newSpaces
    exp <- parseExp
    space
    return Val {n = name, e = exp}
