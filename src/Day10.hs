module Day10 where

import Data.List (uncons)
import Data.Either (fromRight)

import Lib

data ParseResult = Ok | SyntaxError Char | Incomplete String deriving (Show)

isSyntaxError :: ParseResult -> Bool
isSyntaxError (SyntaxError _) = True
isSyntaxError _ = False

isIncomplete :: ParseResult -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _ = False

completionVal :: Char -> Int
completionVal '(' = 1
completionVal '[' = 2
completionVal '{' = 3
completionVal '<' = 4

score :: ParseResult -> Int
score (SyntaxError ')') = 3
score (SyntaxError ']') = 57
score (SyntaxError '}') = 1197
score (SyntaxError '>') = 25137
score (Incomplete s) = foldl (\s c -> 5 * s + completionVal c) 0 s
score Ok = 0

closer :: Char -> Char
closer '(' = ')'
closer '[' = ']'
closer '{' = '}'
closer '<' = '>'

_checkSyntax :: [Char] -> String -> ParseResult
_checkSyntax [] [] = Ok
_checkSyntax stack [] = Incomplete stack
_checkSyntax stack (c:cs) 
	| c `elem` "({[<" = _checkSyntax (c:stack) cs
	| c `elem` ")}]>" = case uncons stack of
		Just (t,ts) | c == closer t -> _checkSyntax ts cs
		otherwise     -> SyntaxError c 
	| otherwise       = SyntaxError c

checkSyntax :: String -> ParseResult
checkSyntax = _checkSyntax []

_useRight :: Either a b -> b
_useRight (Right x) = x

solveIO :: Bool -> IO Int
solveIO False = sum . map score . filter isSyntaxError . map checkSyntax <$> readInput 10 1
solveIO True = _useRight . median . map score . filter isIncomplete . map checkSyntax <$> readInput 10 1

soln :: Lib.Day
soln = Lib.Day 10 (solveIO False) (solveIO True)