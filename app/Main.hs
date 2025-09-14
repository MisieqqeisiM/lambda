module Main (main) where

import AST
import qualified Execution
import qualified ExecutionFast
import Parser (parse)

main :: IO ()
main = do
  -- code <- readFile "example.txt"
  -- print $ refine <$> parse code
  print $ Execution.test
