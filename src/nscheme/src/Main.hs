module Main where

import EmitVS (emitVS)
import Parser (parseNDef)
import SchemeConvert (desugar)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)
import Match (unrollMatches)

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then do
      fileContent <- readFile $ head args
      let out = parse parseNDef (head args) fileContent
      -- print out
      case out of
        Left e -> putStrLn (errorBundlePretty e)
        Right ast -> emitVS $ desugar $ unrollMatches ast
      return ()
    else do
      return ()