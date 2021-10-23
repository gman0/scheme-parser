module Scheme.Eval where

import Scheme.Value
import Scheme.Primitives

import Control.Monad
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool True -> eval conseq
    Bool False -> eval alt
    _ -> throwError $ TypeMismatch "boolean" result
eval (List (Atom func : args)) = mapM eval args >>= apply func

