module Scheme.Eval where

import Control.Monad
import Control.Monad.Except
import Scheme.Primitives
import Scheme.Value

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  evalBoolPredicate pred (return conseq) (return alt)
eval (List (Atom "cond" : clauses)) = do
  clauseExpressions <- findCondClause clauses
  evalCondExpressions clauseExpressions
eval (List (Atom func : args)) = mapM eval args >>= apply func

evalBoolPredicate :: LispVal -> ThrowsError LispVal -> ThrowsError LispVal -> ThrowsError LispVal
evalBoolPredicate pred conseq alt = do
  result <- eval pred
  case result of
    Bool True -> conseq
    Bool False -> alt
    _ -> throwError $ TypeMismatch "boolean" result

findCondClause :: [LispVal] -> ThrowsError LispVal
findCondClause [] = throwError $ Default "non-exhaustive `cond`"
findCondClause (List [Atom "else"] : _) = throwError $ Default "missing expression after `else`"
findCondClause [List (Atom "else" : expressions)] = return $ List expressions
findCondClause (List (Atom "else" : _) : _) = throwError $ Default "`else` must be last clause"
findCondClause (List (pred@(List (Atom func : args)) : expressions) : clauses) =
  let conseq = return $ List $ getOrMakeList (Bool True) expressions
        where
          getOrMakeList x [] = [x]
          getOrMakeList _ xs = xs
      alt = findCondClause clauses
   in evalBoolPredicate pred conseq alt

evalCondExpressions :: LispVal -> ThrowsError LispVal
evalCondExpressions (List [x]) = eval x
evalCondExpressions (List (x : xs)) = do
  _ <- eval x
  evalCondExpressions $ List xs
