{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Primitives where

import Control.Monad
import Control.Monad.Except
import Data.Foldable
import Data.Functor
import Scheme.Value
import Text.ParserCombinators.Parsec hiding (spaces)

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinOp (+)),
    ("-", numericBinOp (-)),
    ("*", numericBinOp (*)),
    ("/", numericBinOp div),
    ("mod", numericBinOp mod),
    ("quotient", numericBinOp quot),
    ("remainder", numericBinOp rem),
    ("symbol?", typeTest TestAtom),
    ("string?", typeTest TestString),
    ("number?", typeTest TestNumber),
    ("symbol->string", castSymbolToString),
    ("string->symbol", castStringToSymbol),
    ("=", numBoolBinOp (==)),
    ("<", numBoolBinOp (<)),
    (">", numBoolBinOp (>)),
    ("/=", numBoolBinOp (/=)),
    (">=", numBoolBinOp (>=)),
    ("<=", numBoolBinOp (<=)),
    ("&&", boolBoolBinOp (&&)),
    ("||", boolBoolBinOp (||)),
    ("string=?", strBoolBinOp (==)),
    ("string?", strBoolBinOp (>)),
    ("string<=?", strBoolBinOp (<=)),
    ("string>=?", strBoolBinOp (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinOp unpackNum

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b) = return $ show b
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

data TypeTestTag = TestAtom | TestString | TestNumber

typeTest :: TypeTestTag -> [LispVal] -> ThrowsError LispVal
typeTest TestAtom [Atom _] = return $ Bool True
typeTest TestString [String _] = return $ Bool True
typeTest TestNumber [Number _] = return $ Bool True
typeTest _ _ = return $ Bool False

castSymbolToString :: [LispVal] -> ThrowsError LispVal
castSymbolToString [Atom val] = return $ String val

castStringToSymbol :: [LispVal] -> ThrowsError LispVal
castStringToSymbol [String val] = return $ Atom val

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ x : xs, List $ y : ys]
eqv [List arg1, List arg2] =
  return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) =
      case eqv [x1, x2] of
        Left err -> False
        Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals (DottedList xs x) (DottedList ys y) unpacker =
  unpackEquals (List $ x : xs) (List $ y : ys) unpacker
unpackEquals (List arg1) (List arg2) unpacker =
  return $ (length arg1 == length arg2) && all unpackEqPair (zip arg1 arg2)
  where
    unpackEqPair (x1, x2) =
      case unpackEquals x1 x2 unpacker of
        Left _ -> False
        Right val -> val
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  ( do
      unpacked1 <- unpacker arg1
      unpacked2 <- unpacker arg2
      return $ unpacked1 == unpacked2
  )
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] =
  mapM (unpackEquals arg1 arg2) unpackers
    >>= \equalResults -> return . Bool . or $ equalResults
  where
    unpackers = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
equal badArgList = throwError $ NumArgs 2 badArgList
