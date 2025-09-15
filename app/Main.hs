module Main (main) where

import AST
import ExTerm (compile)
import Parser (parse)

main :: IO ()
main = do
  code <- readFile "example.txt"
  print (compile . refine <$> parse code)
