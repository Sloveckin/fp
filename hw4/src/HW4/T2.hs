{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  , pCharExact
  , pF
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char (digitToInt)
import HW4.Types
import HW4.T1 (ExceptState(..))
import qualified Data.Char
import GHC.Float (int2Double)
import Data.Scientific

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES r)) s = case r (0, s) of
  Error e          -> Error e
  Success (a :# _) -> Success a

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P (ES r)) (P (ES g)) = P $ ES $ \input ->
      case r input of
        Error _    -> g input
        Success s1 -> Success s1

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success $ () :# (pos, s)
    _  -> Error $ ErrorAtPos pos

pCharExact :: Char -> Parser Char
pCharExact char = mfilter (== char) pChar

pSkipWhiteSpace :: Parser String
pSkipWhiteSpace = many $ pCharExact ' '

pE :: Parser Expr
pE = do
  t <- pT
  pE' t <|> return t

pE' :: Expr -> Parser Expr
pE' = p' '+' '-'

pT :: Parser Expr
pT = do
  t <- pSkipWhiteSpace *> pF <* pSkipWhiteSpace
  pT' t <|> return t

pT' :: Expr -> Parser Expr
pT' = p' '*' '/'

p' :: Char -> Char -> Expr -> Parser Expr
p' ch1 ch2 lastExpr = do
  op <- pSkipWhiteSpace *> (pCharExact ch1 <|> pCharExact ch2) <* pSkipWhiteSpace
  pGetOperation lastExpr op

pGetOperation :: Expr -> Char -> Parser Expr 
pGetOperation lastExpr ch = do
  case ch of
    '*' -> pTerminal lastExpr Mul pF pT'
    '/' -> pTerminal lastExpr Div pF pT'
    '+' -> pTerminal lastExpr Add pT pE'
    '-' -> pTerminal lastExpr Sub pT pE'
    _   -> empty 

pTerminal :: Expr -> (Expr -> Expr -> Prim Expr) -> Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
pTerminal lastExpr operation next recF = do
  t <- next
  recF (Op $ operation lastExpr t) <|> return (Op $ operation lastExpr t)

pF :: Parser Expr
pF =
  let epxr = pCharExact '(' *> pE <* pCharExact ')'
      n    = Val <$> pN
  in  (n <|> epxr) <* pSkipWhiteSpace

pN :: Parser Double
pN = parseWithDot <|> parseWithoutDot

parseWithDot :: Parser Double
parseWithDot = do
  first  <- pNumber
  _ <- pCharExact '.'
  second <- pNumber
  return $ int2Double (convertStringToInt first) + convertStringToFract second

convertStringToFract :: String -> Double
convertStringToFract s =
  let t = toInteger $ convertStringToInt s
  in toRealFloat $ scientific t (- length s)

parseWithoutDot :: Parser Double
parseWithoutDot = do
  int2Double . convertStringToInt <$> pNumber

convertStringToInt :: String -> Int
convertStringToInt = foldl (\acc digit -> acc * 10 + digitToInt digit) 0

pNumber :: Parser String
pNumber = some $ mfilter Data.Char.isDigit pChar

-- No metohds
instance MonadPlus Parser

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ pE <* pEof
