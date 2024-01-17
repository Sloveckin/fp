module HW5.Pretty
  ( prettyValue,
  ) where

import Prettyprinter (Doc, Pretty(pretty), viaShow, list, encloseSep, parens, dquotes)
import Prettyprinter.Render.Terminal (AnsiStyle)
import HW5.Base ( HiValue(..), HiValue, HiFun (..), HiAction (..) )
import Data.Scientific (floatingOrInteger, fromRationalRepetendUnlimited)
import Data.Ratio
import Data.Foldable
import Data.Sequence (Seq)
import Numeric (showHex)
import Data.ByteString as BS
import Data.Word (Word8)
import qualified Data.Map as Map

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueAction (HiActionWrite path t)) = pretty "write" <> prettySeqOfHiValue "(" ")" [viaShow path, prettyValue $ HiValueBytes t] 
prettyValue (HiValueFunction HiFunNotGreaterThan)  = pretty "not-greater-than"
prettyValue (HiValueAction (HiActionMkDir path))   = pretty "mkdir" <> parens (viaShow path)
prettyValue (HiValueAction (HiActionEcho text))    = pretty "echo" <> parens (viaShow text)
prettyValue (HiValueAction (HiActionRand x y))     = pretty "rand" <> parens (pretty x <> pretty ", " <> pretty y)
prettyValue (HiValueAction (HiActionRead path))    = pretty "read" <> parens (viaShow path)
prettyValue (HiValueAction (HiActionChDir path))   = pretty "cd" <> parens (viaShow path)
prettyValue (HiValueFunction HiFunDeserialise)     = pretty "deserialise"
prettyValue (HiValueFunction HiFunUnpackBytes)     = pretty "unpack-bytes"
prettyValue (HiValueFunction HiFunNotLessThan)     = pretty "not-less-than"
prettyValue (HiValueFunction HiFunGreaterThan)     = pretty "greater-than"
prettyValue (HiValueFunction HiFunDecodeUtf8)      = pretty "decode-utf8"
prettyValue (HiValueFunction HiFunEncodeUtf8)      = pretty "encode-utf8"
prettyValue (HiValueFunction HiFunNotEquals)       = pretty "not-equals"
prettyValue (HiValueFunction HiFunParseTime)       = pretty "parse-time"
prettyValue (HiValueFunction HiFunPackBytes)       = pretty "pack-bytes"
prettyValue (HiValueFunction HiFunSerialise)       = pretty "serialise"
prettyValue (HiValueFunction HiFunLessThan)        = pretty "less-than"
prettyValue (HiValueFunction HiFunReverse)         = pretty "reverse"
prettyValue (HiValueFunction HiFunToUpper)         = pretty "to-upper"
prettyValue (HiValueFunction HiFunToLower)         = pretty "to-lower"
prettyValue (HiValueFunction HiFunEquals)          = pretty "equals"
prettyValue (HiValueFunction HiFunValues)          = pretty "values"
prettyValue (HiValueFunction HiFunLength)          = pretty "length"
prettyValue (HiValueFunction HiFunInvert)          = pretty "invert"
prettyValue (HiValueFunction HiFunRange)           = pretty "range"
prettyValue (HiValueFunction HiFunChDir)           = pretty "cd"
prettyValue (HiValueFunction HiFunMkDir)           = pretty "mkdir"
prettyValue (HiValueFunction HiFunCount)           = pretty "count"
prettyValue (HiValueFunction HiFunWrite)           = pretty "write"
prettyValue (HiValueFunction HiFunRand)            = pretty "rand"
prettyValue (HiValueFunction HiFunRead)            = pretty "read"
prettyValue (HiValueFunction HiFunKeys)            = pretty "keys"
prettyValue (HiValueFunction HiFunEcho)            = pretty "echo"
prettyValue (HiValueFunction HiFunAdd)             = pretty "add"
prettyValue (HiValueFunction HiFunMul)             = pretty "mul"
prettyValue (HiValueFunction HiFunSub)             = pretty "sub"
prettyValue (HiValueFunction HiFunDiv)             = pretty "div"
prettyValue (HiValueFunction HiFunNot)             = pretty "not"
prettyValue (HiValueFunction HiFunAnd)             = pretty "and"
prettyValue (HiValueFunction HiFunTrim)            = pretty "trim"
prettyValue (HiValueFunction HiFunList)            = pretty "list"
prettyValue (HiValueFunction HiFunFold)            = pretty "fold"
prettyValue (HiValueAction HiActionNow)            = pretty "now"
prettyValue (HiValueAction HiActionCwd)            = pretty "cwd"
prettyValue (HiValueFunction HiFunOr)              = pretty "or"
prettyValue (HiValueFunction HiFunIf)              = pretty "if"
prettyValue (HiValueFunction HiFunZip)             = pretty "zip"
prettyValue (HiValueFunction HiFunUnzip)           = pretty "unzip"
prettyValue (HiValueBool False)                    = pretty "false" 
prettyValue (HiValueString str)                    = viaShow str
prettyValue (HiValueBool True)                     = pretty "true" 
prettyValue (HiValueTime time)                     = pretty "parse-time" <> parens (dquotes $ viaShow time)
prettyValue (HiValueList arg)                      = prettyList arg
prettyValue HiValueNull                            = pretty "null"
prettyValue (HiValueNumber val)                    = case fromRationalRepetendUnlimited val of
                                                          (x, Nothing) -> either pretty pretty (floatingOrInteger x :: Either Double Integer)
                                                          _            -> pretty $ frac (numerator val) (denominator val)
prettyValue (HiValueDict mp) = 
  let t = Map.toList mp 
      d = Prelude.map (\(x, y) -> prettyValue x <> pretty ": " <> prettyValue y) t
    in prettySeqOfHiValue "{ " " }" d
prettyValue (HiValueBytes bytes)                   = let arr = pretty <$> Prelude.map word8ToString (BS.unpack bytes)  
                                                     in case arr of 
                                                      [] -> pretty "[# #]" 
                                                      x  -> prettySeqOfHiValueBase "[# " " #]" " " x

word8ToString :: Word8 -> String
word8ToString word = let s = showHex word "" in if Prelude.length s == 1 then "0" ++ s else s

prettyList :: Seq HiValue -> Doc AnsiStyle
prettyList arg =
  let t = toList arg
  in list $ fmap prettyValue t


frac :: Integer -> Integer -> String
frac a b = if abs a < abs b then show a ++ "/" ++ show b
           else
            let (x, y) = quotRem a b
                sign = if a * b > 0 then " + " else " - "
            in show x ++ sign ++ show (abs y) ++ "/" ++ show b

prettySeqOfHiValue :: String -> String -> [Doc AnsiStyle] -> Doc AnsiStyle 
prettySeqOfHiValue first second = prettySeqOfHiValueBase first second  ", " 

prettySeqOfHiValueBase :: String -> String -> String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettySeqOfHiValueBase first second sep = encloseSep (pretty first) (pretty second) (pretty sep)