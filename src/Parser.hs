module Parser where

import Control.Monad.State
import Data.Char (isDigit)
import Text.Printf

type Parser = State String

grab :: Int -> Parser String
grab i = state $ splitAt i

grabNum :: (Read a, Num a) => Parser a
grabNum = do
  cs <- get
  let nums = takeWhile (\c -> isDigit c || c == '-') cs
  let cs' = drop (length nums) cs
  when (null nums) $ error "Expected digits"
  put cs'
  return . read $ nums

expect :: String -> Parser ()
expect xs = do
  cs <- get
  let (as,ps) = splitAt (length xs) cs
  when (as /= xs) $ error $ printf "Expected \"%s\", got \"%s\"" xs as
  put ps

runParser :: Parser a -> String -> a
runParser = evalState