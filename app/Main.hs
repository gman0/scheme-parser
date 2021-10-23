module Main where

import Scheme.Value
import Scheme.Eval
import Scheme.Primitives

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
