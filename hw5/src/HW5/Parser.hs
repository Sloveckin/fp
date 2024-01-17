module HW5.Parser
  ( parse
  ) where

import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec ( Parsec, between,  choice, manyTill, sepBy, runParser, MonadParsec (eof, try, notFollowedBy), satisfy, sepBy1)
import Text.Megaparsec.Char (char, string, space1, space)
import HW5.Base ( HiValue(..), HiExpr(..), HiFun(..), HiAction (..) )
import Control.Applicative
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text as T hiding (empty)
import Data.Word (Word8)
import Data.ByteString as BT hiding (empty)
import Data.Char (isAlphaNum, isAlpha)
import Data.List (intercalate)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pNumber :: Parser HiExpr
pNumber = HiExprValue <$> fmap (HiValueNumber . toRational) (L.signed space L.scientific)

table :: [[Operator Parser HiExpr]]
table = [[binary "*" HiFunMul, binary' "/" HiFunDiv],
         [binary "+" HiFunAdd, binary "-" HiFunSub],
         [nonAssotiative' "<" HiFunLessThan, nonAssotiative' ">" HiFunGreaterThan,
         nonAssotiative "<=" HiFunNotGreaterThan, nonAssotiative ">=" HiFunNotLessThan,
         nonAssotiative "==" HiFunEquals, nonAssotiative "/=" HiFunNotEquals],
         [logic "&&" HiFunAnd],
         [logic "||" HiFunOr]]

pTerm :: Parser HiExpr
pTerm = choice [pFunWithArgs, parenthesis pExpr]

parenthesis :: Parser a -> Parser a
parenthesis = between (symbol "(") (symbol ")")

squareBracktes :: Parser a -> Parser a
squareBracktes = between (symbol "[") (symbol "]")

byteBractes :: Parser a -> Parser a
byteBractes = between (symbol "[#") (symbol "#]")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

pFun :: Parser HiFun
pFun = lexeme $ choice [HiFunAdd           <$ string "add"
                      ,HiFunSub            <$ string "sub"
                      ,HiFunMul            <$ string "mul"
                      ,HiFunDiv            <$ string "div"
                      ,HiFunNotEquals      <$ string "not-equals"
                      ,HiFunNotLessThan    <$ string "not-less-than"
                      ,HiFunNotGreaterThan <$ string "not-greater-than"
                      ,HiFunNot            <$ string "not"
                      ,HiFunAnd            <$ string "and"
                      ,HiFunOr             <$ string "or"
                      ,HiFunEquals         <$ string "equals"
                      ,HiFunLessThan       <$ string "less-than"
                      ,HiFunGreaterThan    <$ string "greater-than"
                      ,HiFunIf             <$ string "if"
                      ,HiFunLength         <$ string "length"
                      ,HiFunToUpper        <$ string "to-upper"
                      ,HiFunToLower        <$ string "to-lower"
                      ,HiFunReverse        <$ string "reverse"
                      ,HiFunTrim           <$ string "trim"
                      ,HiFunToUpper        <$ string "to-upper"
                      ,HiFunList           <$ string "list"
                      ,HiFunRange          <$ string "range"
                      ,HiFunFold           <$ string "fold"
                      ,HiFunPackBytes      <$ string "pack-bytes"
                      ,HiFunUnpackBytes    <$ string "unpack-bytes"
                      ,HiFunZip            <$ string "zip"
                      ,HiFunUnzip          <$ string "unzip"
                      ,HiFunEncodeUtf8     <$ string "encode-utf8"
                      ,HiFunDecodeUtf8     <$ string "decode-utf8"
                      ,HiFunSerialise      <$ string "serialise"
                      ,HiFunDeserialise    <$ string "deserialise"
                      ,HiFunRead           <$ string "read"
                      ,HiFunWrite          <$ string "write"
                      ,HiFunMkDir          <$ string "mkdir"
                      ,HiFunChDir          <$ string "cd"
                      ,HiFunParseTime      <$ string "parse-time"
                      ,HiFunRand           <$ string "rand"
                      ,HiFunEcho           <$ string "echo"
                      ,HiFunCount          <$ string "count"
                      ,HiFunKeys           <$ string "keys"
                      ,HiFunValues         <$ string "values"
                      ,HiFunInvert         <$ string "invert"]

pBool :: Parser HiExpr
pBool = HiExprValue . HiValueBool <$> choice [True <$ string "true", False <$ string "false"]

pValue :: Parser HiExpr
pValue = choice [pNumber, pCwdOrNow, pFunName, pBool, pString, pNull, try pList, pBytes, pDict]

pCwdOrNow :: Parser HiExpr
pCwdOrNow = HiExprValue . HiValueAction <$> choice [HiActionCwd <$ string "cwd" ,HiActionNow <$ string "now"]

pNull :: Parser HiExpr
pNull =  lexeme $ HiExprValue <$> (HiValueNull <$ string "null")

pFunName :: Parser HiExpr
pFunName = fmap (HiExprValue . HiValueFunction) pFun

pString :: Parser HiExpr
pString = fmap (HiExprValue . HiValueString . T.pack)  (char '"' *> manyTill L.charLiteral (char '"'))

pExpr :: Parser HiExpr
pExpr = makeExprParser (sc *> pTerm <* sc) table

pFunWithArgs :: Parser HiExpr
pFunWithArgs = do
  name <- pValue  <|> parenthesis pExpr 
  try $ pBrackets name

pBrackets :: HiExpr -> Parser HiExpr
pBrackets name = do
  args <- pFunArgs
  t <- optional $ symbol "!"
  let appl = Prelude.foldl HiExprApply name args
  case t of
    Nothing -> return appl 
    Just _  -> return $ HiExprRun appl

pFunArgs :: Parser [[HiExpr]]
pFunArgs = many $ pDot <|> parenthesis (lexeme $ (pExpr <* sc) `sepBy` symbol ",")

pDot :: Parser [HiExpr]
pDot = do
    _ <-symbol "."
    arg <- afterDot
    return [HiExprValue arg]

pDict :: Parser HiExpr
pDict = fmap HiExprDict pDictArg

pKeyValue :: Parser (HiExpr, HiExpr)
pKeyValue = do
  key <- pExpr
  _ <- symbol ":"
  value <- pValue
  return (key, value)

pList :: Parser HiExpr
pList = fmap (HiExprApply $ HiExprValue $ HiValueFunction HiFunList)  pListArgs

pListArgs :: Parser [HiExpr]
pListArgs = squareBracktes $ lexeme (pExpr <* sc) `sepBy` symbol ","

afterDot :: Parser HiValue
afterDot = do
  t <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return $ HiValueString $ T.pack (Data.List.intercalate "-" t) 

pDictArg :: Parser [(HiExpr, HiExpr)]
pDictArg = curlyBraces $ lexeme $ (pKeyValue <* sc) `sepBy` symbol ","

pBytes :: Parser HiExpr
pBytes = fmap (fmap HiExprValue HiValueBytes . BT.pack) pWords8

pWords8 :: Parser [Word8]
pWords8 = byteBractes $ many $ lexeme L.hexadecimal

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (sc *> pExpr <* eof) ""

binary :: String -> HiFun -> Operator Parser HiExpr
binary name f = InfixL $ apply f <$ symbol name

binary' :: String -> HiFun -> Operator Parser HiExpr
binary' name f = InfixL $ try $ apply f <$ symbol name <* notFollowedBy (symbol "=")

nonAssotiative :: String -> HiFun -> Operator Parser HiExpr
nonAssotiative name f = InfixN $ apply f <$ symbol name

nonAssotiative' :: String -> HiFun -> Operator Parser HiExpr
nonAssotiative' name f = InfixN $ try $ apply f <$ symbol name <* notFollowedBy (symbol "=")

logic :: String -> HiFun -> Operator Parser HiExpr
logic name f = InfixR $ apply f <$ symbol name

apply :: HiFun -> HiExpr -> HiExpr -> HiExpr
apply fun x y = HiExprApply (HiExprValue $ HiValueFunction fun) [x, y]
