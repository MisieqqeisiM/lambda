module Main (main) where

import AST
import ExTerm (compile)
import Execution (run)
import Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      code <- readFile fileName
      print (run . compile . refine <$> parse code)
    ["-exec", fileName] -> do
      code <- readFile fileName
      print (run . compile . refine <$> parse code)
    ["-compile", fileName] -> do
      code <- readFile fileName
      print (compile . refine <$> parse code)
    ["-parse", fileName] -> do
      code <- readFile fileName
      print (refine <$> parse code)
    _ -> print "Usage: lambda-parser [-exec|-compile|-parse] <filename>"
