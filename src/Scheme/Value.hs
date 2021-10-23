module Scheme.Value where

import Control.Monad
import Control.Monad.Except
import Numeric
import Data.Foldable
import Data.Functor
import Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Character Char
  | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool b) = if b then "#t" else "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ many1 (noneOf "\"\\") <|> parseEscSeq
  char '"'
  return $ String (concat x)

parseEscSeq :: Parser String
parseEscSeq = do
  char '\\'
  x <- oneOf "ntr\"\\"
  return $ case x of
    'n' -> "\n"
    't' -> "\t"
    'r' -> "\r"
    _ -> [x]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Atom (first : rest)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  x <- oneOf "tf"
  return $
    Bool $ case x of
      't' -> True
      'f' -> False

parseNumber :: Parser LispVal
parseNumber =
  let numParsers =
        [ parseNumDecimal1,
          parseNumDecimal2,
          parseNumOct,
          parseNumHex,
          parseNumBin
        ]
   in choice numParsers >>= \x -> return x

parseNumDecimal1 :: Parser LispVal
parseNumDecimal1 = Number . read <$> many1 digit

parseNumDecimal2 :: Parser LispVal
parseNumDecimal2 = do
  try $ string "#d"
  x <- many1 digit
  return $ Number (read x)

parseNumOct :: Parser LispVal
parseNumOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dec x)
  where
    oct2dec x = fst $ head $ readOct x

parseNumHex :: Parser LispVal
parseNumHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dec x)
  where
    hex2dec x = fst $ head $ readHex x

parseNumBin :: Parser LispVal
parseNumBin = do
  try $ string "#b"
  x <- many1 (oneOf "01")
  return $ Number (bin2dec x)
  where
    bin2dec xs = foldl (\acc x -> 2 * acc + zeroOne x) 0 xs
      where
        zeroOne x = if x == '1' then 1 else 0

parseChar :: Parser LispVal
parseChar = do
  try $ string "#\\"
  x <- namedSpace <|> anyChar
  return $ Character x
  where
    namedSpace = do
      x <- try $ string "space" <|> string "newline"
      return $ case x of
        "space" -> ' '
        "newline" -> '\n'

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  choice
    [ parseAtom,
      parseString,
      parseNumber,
      parseChar,
      parseQuoted,
      parseBool,
      listExpr
    ]
  where
    listExpr = do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args, found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
