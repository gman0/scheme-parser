module Main where

import Scheme.Primitives
import Scheme.Value
import System.Environment
import System.IO

printUsage :: IO String -> IO ()
printUsage progNameM = do
  progName <- progNameM
  putStrLn "Scheme R5RS interpreter\n"
  putStrLn $ "Usage: " ++ progName ++ " [EXPR]\n"
  putStrLn " Positional arguments:\n"
  putStrLn "\t[EXPR]\tOptional. Evaluate Scheme expression.\n"
  putStrLn " If no EXPR is given, interactive Scheme shell will open."

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr =
  let evaled = fmap show $ readExpr expr >>= eval
   in return $ extractValue $ trapError evaled

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = do
  putStrLn "Type \":quit\" or \":q\" to exit the shell."
  until_ (`elem` [":quit", ":q"]) (readPrompt "scm>>> ") evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Error: Program takes only zero or one argument." >> printUsage getProgName
