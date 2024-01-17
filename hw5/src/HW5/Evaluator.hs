module HW5.Evaluator
  ( eval
  ) where

import HW5.Base ( HiExpr(..), HiValue(..), HiError(..), HiFun (..), HiMonad (..), HiAction (..))
import Control.Monad.Trans.Except
import Data.Text as T
import Data.Semigroup (stimes)
import Data.Ratio ()
import Data.Sequence as SQ
import Data.Foldable(toList, foldlM)
import Data.ByteString as BT
import Data.Word (Word8)
import GHC.Real (Ratio(..))
import qualified Data.Text.Encoding as T
import Codec.Compression.Zlib (compressWith, decompress, bestCompression, compressLevel, defaultCompressParams)
import Data.ByteString.Lazy
import Codec.Serialise (serialise, deserialise)
import Text.Read (readMaybe)
import Data.Time (addUTCTime, diffUTCTime, UTCTime)
import qualified Data.Map as Map
import Control.Monad.Trans.Class

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ eval1 expr

eval1 :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval1 (HiExprValue x) = return x
eval1 (HiExprApply funName args) = do 
    name <- eval1 funName
    runFunction name args
eval1 (HiExprRun x) = do
  a <- eval1 x
  case a of
      HiValueAction val -> lift $ runAction val
      _                 -> invalidArgument 
eval1 (HiExprDict arr) = do
  keys   <- mapM (eval1 . fst) arr
  values <- mapM (eval1 . snd) arr
  returnDict $ Map.fromList $ Prelude.zip keys values

runFunction :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
runFunction (HiValueNumber _) _                          = invalidFunction 
runFunction (HiValueBool _) _                            = invalidFunction 
runFunction HiValueNull _                                = invalidFunction 
runFunction (HiValueFunction HiFunIf) [flag, f, g]       = if' flag f g
runFunction (HiValueFunction HiFunList) arg              = list arg
runFunction (HiValueFunction HiFunAnd) [x, y]            = and' x y
runFunction (HiValueFunction HiFunOr)  [x, y]            = or' x y
runFunction funName [x, y]                               = bin funName x y 
runFunction funName [x]                                  = unary funName x 
runFunction _ _                                          = arityMismatch 

unary :: HiMonad m => HiValue -> HiExpr -> ExceptT HiError m HiValue
unary fun x = do 
  arg <- eval1 x
  case fun of
    HiValueFunction HiFunUnpackBytes -> byteUnpack arg
    HiValueFunction HiFunDeserialise -> deser arg
    HiValueFunction HiFunEncodeUtf8  -> encode arg
    HiValueFunction HiFunDecodeUtf8  -> decode arg
    HiValueFunction HiFunParseTime   -> time' arg
    HiValueFunction HiFunSerialise   -> ser arg
    HiValueFunction HiFunPackBytes   -> bytePack arg
    HiValueFunction HiFunToUpper     -> upper arg 
    HiValueFunction HiFunToLower     -> lower arg
    HiValueFunction HiFunReverse     -> reverse' arg
    HiValueFunction HiFunValues      -> values' arg
    HiValueFunction HiFunInvert      -> invert' arg
    HiValueFunction HiFunLength      -> length' arg
    HiValueFunction HiFunCount       -> count' arg
    HiValueFunction HiFunUnzip       -> unzipp arg
    HiValueFunction HiFunChDir       -> cd arg
    HiValueFunction HiFunMkDir       -> mkdir arg
    HiValueFunction HiFunEcho        -> echo arg
    HiValueFunction HiFunTrim        -> trim' arg
    HiValueFunction HiFunKeys        -> keys' arg
    HiValueFunction HiFunRead        -> read' arg
    HiValueFunction HiFunNot         -> not' arg
    HiValueFunction HiFunZip         -> zipp arg
    HiValueString text               -> charAt text arg
    HiValueBytes bytes               -> getElemBytes bytes arg
    HiValueList sq                   -> getElem (toList sq) arg
    HiValueDict mp                   -> getByKey mp arg
    _                                -> arityMismatch 

bin :: HiMonad m => HiValue -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
bin fun x y = do 
  first  <- eval1 x
  second <- eval1 y
  case fun of 
    HiValueFunction HiFunNotGreaterThan -> notGreaterThanOp first second
    HiValueFunction HiFunNotLessThan    -> notLessThanOp first second
    HiValueFunction HiFunGreaterThan    -> lessThan second first
    HiValueFunction HiFunNotEquals      -> notEqualsOp first second
    HiValueFunction HiFunLessThan       -> lessThan first second
    HiValueFunction HiFunEquals         -> eq first second
    HiValueFunction HiFunRange          -> range first second
    HiValueFunction HiFunWrite          -> write' first second
    HiValueFunction HiFunRand           -> rand' first second
    HiValueFunction HiFunFold           -> myFold first second
    HiValueFunction HiFunAdd            -> add first second
    HiValueFunction HiFunDiv            -> div' first second
    HiValueFunction HiFunMul            -> mul first second
    HiValueFunction HiFunSub            -> sub first second
    HiValueString text                  -> subString text first second
    HiValueBytes bytes                  -> subBytes bytes first second
    HiValueList sq                      -> subList sq first second
    _                                   -> arityMismatch 


invert' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
invert' (HiValueDict dic) = 
      let
        lst = Prelude.map (\(a1, b) -> (b, [a1])) (Map.toList dic)
        mp = Prelude.foldl (\acc (a1, b) -> Map.insertWith (++) a1 b acc) Map.empty lst
      in returnDict $ Map.map (HiValueList . SQ.fromList) mp
invert'  _                = invalidArgument 

count' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
count' (HiValueString text) = baseCount (T.unpack text) (\(a1, b) -> (HiValueString $ T.pack [a1], HiValueNumber $ toRational b))
count' (HiValueList sq)     = baseCount (toList sq) (\(a1, b) -> (a1, HiValueNumber $ toRational b))
count' (HiValueBytes bytes) = baseCount (BT.unpack bytes) (\(a1, b) -> (HiValueNumber $ toRational a1, HiValueNumber $ toRational b))
count' _                    = invalidArgument 

baseCount :: (HiMonad m, Ord a) => [a] -> ((a, Integer) -> (HiValue, HiValue)) ->  ExceptT HiError m HiValue
baseCount arr f = let
                  mp = Prelude.foldl (\acc ch -> Map.insertWith (+) ch 1 acc) Map.empty arr
                  m = Prelude.map f (Map.toList mp)
              in returnDict $ Map.fromList m

keys' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
keys' (HiValueDict mp) = returnList $ fromList $ Map.keys mp
keys' _                = invalidArgument 

values' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
values' (HiValueDict mp) = returnList $ fromList $ Map.elems mp
values' _                = invalidArgument 

getByKey :: HiMonad m => Map.Map HiValue HiValue -> HiValue -> ExceptT HiError m HiValue
getByKey mp key = maybe returnNull return (Map.lookup key mp)

echo :: HiMonad m => HiValue -> ExceptT HiError m HiValue
echo (HiValueString text) = returnAction $ HiActionEcho text
echo _                    = invalidArgument 

time' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
time' (HiValueString str) = maybe returnNull returnTime (readMaybe $ T.unpack str)
time' _                   = invalidArgument 

rand' :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
rand' (HiValueNumber (a :% 1)) (HiValueNumber (b :% 1)) = returnAction $ HiActionRand (fromIntegral a) (fromIntegral b)
rand' _ _                                               = invalidArgument 

cd :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cd (HiValueString text) = returnAction $ HiActionChDir $ T.unpack text
cd _                    = invalidArgument 

mkdir :: HiMonad m => HiValue -> ExceptT HiError m HiValue
mkdir (HiValueString text) = returnAction $ HiActionMkDir $ T.unpack text
mkdir _                    = invalidArgument 

read' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
read' (HiValueString text) = returnAction $ HiActionRead $ T.unpack text
read' _                    = invalidArgument 

write' :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
write' (HiValueString path) b = encode' b >>= returnAction . HiActionWrite (T.unpack path) 
write' _ _                    = invalidArgument 

charAt :: HiMonad m => Text -> HiValue -> ExceptT HiError m HiValue
charAt text idx = el (T.unpack text) idx (HiValueString . T.singleton)

getElem :: HiMonad m => [HiValue] -> HiValue -> ExceptT HiError m HiValue
getElem arr idx = el arr idx id

getElemBytes :: HiMonad m => BT.ByteString -> HiValue -> ExceptT HiError m HiValue
getElemBytes bytes idx = el (BT.unpack bytes) idx (HiValueNumber . toRational)


el :: HiMonad m => [a] -> HiValue -> (a -> HiValue) ->  ExceptT HiError m HiValue
el arr (HiValueNumber (x :% 1)) f = if x < fromIntegral (Prelude.length arr) && x >= 0
                                          then return $ f $ arr !! fromInteger x
                                          else returnNull
el _ _ _                          = invalidArgument 

subString :: HiMonad m => Text -> HiValue -> HiValue -> ExceptT HiError m HiValue
subString text (HiValueNumber a) (HiValueNumber b) = runSubString a b text
subString text HiValueNull (HiValueNumber val)     = runSubString 0 val text
subString text (HiValueNumber val) HiValueNull     = runSubString val (toRational $ T.length text) text
subString _ _ _                                    = invalidArgument 

runSubString :: HiMonad m => Rational -> Rational -> Text -> ExceptT HiError m HiValue
runSubString a b text = sub' a b (T.unpack text) (HiValueString . T.pack)

subList :: HiMonad m => Seq HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
subList sq (HiValueNumber a) (HiValueNumber b) = runSubList a b sq
subList sq HiValueNull (HiValueNumber val)     = runSubList 0 val sq
subList sq (HiValueNumber val) HiValueNull     = runSubList val (toRational $ SQ.length sq) sq
subList _ _ _                                  = invalidArgument 

runSubList :: HiMonad m => Rational -> Rational -> Seq HiValue -> ExceptT HiError m HiValue
runSubList a b sq = sub' a b (toList sq) (HiValueList . fromList)

subBytes :: HiMonad m => BT.ByteString -> HiValue -> HiValue -> ExceptT HiError m HiValue
subBytes bytes (HiValueNumber a) (HiValueNumber b) = runSubBytes a b bytes
subBytes bytes HiValueNull (HiValueNumber val)     = runSubBytes 0 val bytes
subBytes bytes (HiValueNumber val) HiValueNull     = runSubBytes val (toRational $ BT.length bytes) bytes
subBytes _ _ _                                     = invalidArgument 

runSubBytes :: HiMonad m => Rational -> Rational -> BT.ByteString -> ExceptT HiError m HiValue
runSubBytes a b bytes = sub' a b (BT.unpack bytes) (HiValueBytes . BT.pack)

sub' :: HiMonad m => Rational -> Rational -> [a] -> ([a] -> HiValue) -> ExceptT HiError m HiValue
sub' (a :% 1) (b :% 1) arr con = let
   len = Prelude.length arr
   a' = fromIntegral a
   b' = fromIntegral b
   in return $ con $ Prelude.take (index' len b' - index' len a') $ Prelude.drop (index' len a') arr
sub' _ _ _ _                   = invalidArgument 

index' :: Int -> Int -> Int
index' len x = if x >= 0
                      then x
                      else x + len

div' :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
div' (HiValueNumber _) (HiValueNumber 0) = throwE HiErrorDivideByZero
div' (HiValueString a) (HiValueString b) = returnString $ (\n k -> n <> T.pack "/" <> k) a b
div' (HiValueNumber a) (HiValueNumber b) = returnNumber $ a / b
div' _ _                                 = invalidArgument 

add :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
add (HiValueNumber a) (HiValueNumber b) = returnNumber $ a + b
add (HiValueString a) (HiValueString b) = returnString $ T.append a b
add (HiValueBytes a) (HiValueBytes b)   = returnBytes $ BT.append a b
add (HiValueTime a) (HiValueNumber b)   = returnTime $ addUTCTime (fromRational b) a
add (HiValueNumber a) (HiValueTime b)   = returnTime $ addUTCTime (fromRational a) b
add (HiValueList a) (HiValueList b)     = returnList $ a >< b
add  _ _                                = invalidArgument 

sub :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
sub (HiValueNumber a) (HiValueNumber b) = returnNumber $ a - b
sub (HiValueTime a) (HiValueTime b)     = returnNumber $ toRational $ diffUTCTime a b
sub _ _                                 = invalidArgument 

mul :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
mul (HiValueNumber (a :% 1)) (HiValueString text) = returnString $ stimes a text 
mul (HiValueNumber (a :% 1)) (HiValueBytes bytes) = returnBytes $ stimes a bytes 
mul (HiValueString text) (HiValueNumber (a :% 1)) = returnString $ stimes a text 
mul (HiValueBytes bytes) (HiValueNumber (a :% 1)) = returnBytes $ stimes a bytes 
mul (HiValueList sq) (HiValueNumber (a :% 1))     = returnList $ fromList $ stimes a (toList sq)
mul (HiValueNumber (a :% 1)) (HiValueList sq)     = returnList $ fromList $ stimes a (toList sq) 
mul (HiValueNumber f) (HiValueNumber g)           = returnNumber $ f * g
mul _ _                                           = invalidArgument 

or' :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
or' x y = do
  a <- eval1 x
  case a of
    HiValueNull       -> eval1 y
    HiValueBool False -> eval1 y
    _                 -> return a 

and' :: HiMonad m => HiExpr -> HiExpr -> ExceptT HiError m HiValue
and' x y = do
  a <- eval1 x
  case a of
    HiValueNull       -> returnNull
    HiValueBool False -> monadFalse
    _                 -> eval1 y

not' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
not' (HiValueBool val) = returnBool $ not val
not' _                 = invalidArgument 

eq :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
eq = monadF (==) 

notEqualsOp :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
notEqualsOp x y = eq x y >>= not' 

lessThan :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
lessThan (HiValueBool _) (HiValueNumber _) = monadTrue 
lessThan (HiValueNumber _) (HiValueBool _) = monadFalse
lessThan x y                               = monadLess x y

notLessThanOp :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
notLessThanOp x y = lessThan x y >>= not' 

if' :: HiMonad m => HiExpr -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
if' flag f g = do
  flagRes <- eval1 flag
  case flagRes of
    HiValueBool True  -> eval1 f
    HiValueBool False -> eval1 g
    _                 -> invalidArgument 

list :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
list arr = mapM eval1 arr >>= (returnList . fromList)

range :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
range (HiValueNumber a) (HiValueNumber b) = returnList $ HiValueNumber <$> fromList [a..b]
range _ _                                 = invalidArgument 

myFold :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
myFold name (HiValueList (first :<| val)) = foldlM (helpMyFold name) first val
myFold _ (HiValueList Empty)              = returnNull
myFold _ _                                = invalidArgument 

helpMyFold :: HiMonad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
helpMyFold fun x y = runFunction fun [HiExprValue x, HiExprValue y]

notGreaterThanOp :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
notGreaterThanOp x y = lessThan y x >>= not' 

length' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
length' (HiValueString text) = returnNumber $ toRational $ T.length text
length' (HiValueList arr)    = returnNumber $ toRational $ SQ.length arr
length' _                    = invalidArgument 

upper :: HiMonad m => HiValue -> ExceptT HiError m HiValue
upper = stringFunBase T.toUpper

lower :: HiMonad m => HiValue -> ExceptT HiError m HiValue
lower = stringFunBase T.toLower

reverse' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
reverse' (HiValueString text) = returnString $ T.reverse text
reverse' (HiValueList arr)    = returnList $ SQ.reverse arr
reverse'   _                  = invalidArgument 

trim' :: HiMonad m => HiValue -> ExceptT HiError m HiValue
trim' = stringFunBase T.strip

stringFunBase :: HiMonad m => (Text -> Text) -> HiValue -> ExceptT HiError m HiValue
stringFunBase f (HiValueString text) = returnString $ f text
stringFunBase _ _                    = invalidArgument 

packToWord8 :: HiMonad m => HiValue -> ExceptT HiError m Word8
packToWord8 (HiValueNumber (val :% 1)) = if val >= 0 && val <= 255
                                         then return $ fromInteger val
                                         else invalidArgument 
packToWord8 _                          = invalidArgument 

bytePack :: HiMonad m => HiValue -> ExceptT HiError m HiValue
bytePack (HiValueList t) = do
          res <- mapM packToWord8 (toList t)
          returnBytes $ BT.pack res
bytePack  _              = invalidArgument 

byteUnpack :: HiMonad m => HiValue -> ExceptT HiError m HiValue
byteUnpack (HiValueBytes bytes) =
        let t = Prelude.map (HiValueNumber . toRational) (BT.unpack bytes)
        in returnList $ fromList t
byteUnpack _                    = invalidArgument 

encode :: HiMonad m => HiValue -> ExceptT HiError m HiValue
encode x = encode' x >>= returnBytes

encode' :: HiMonad m => HiValue -> ExceptT HiError m BT.ByteString
encode' (HiValueString text) = return $ T.encodeUtf8 text
encode' _                    = invalidArgument 

decode :: HiMonad m => HiValue -> ExceptT HiError m HiValue
decode (HiValueBytes bytes) = case T.decodeUtf8' bytes of
                                     Right text -> returnString text
                                     Left _     -> returnNull
decode _                    = invalidArgument 

zipp :: HiMonad m => HiValue -> ExceptT HiError m HiValue
zipp (HiValueBytes bytes) = returnBytes $ toStrict $ compressWith defaultCompressParams {compressLevel = bestCompression} (fromStrict bytes)
zipp _                    = invalidArgument 

unzipp :: HiMonad m => HiValue -> ExceptT HiError m HiValue
unzipp (HiValueBytes bytes) = returnBytes $ toStrict $ decompress (fromStrict bytes)
unzipp _                    = invalidArgument 

ser :: HiMonad m => HiValue -> ExceptT HiError m HiValue
ser x = returnBytes $ toStrict $ serialise x

deser :: HiMonad m => HiValue -> ExceptT HiError m HiValue
deser (HiValueBytes bytes) = return $ deserialise (fromStrict bytes)
deser _                    = invalidArgument 

monadTrue :: HiMonad m => ExceptT HiError m HiValue
monadTrue = returnBool True

monadFalse :: HiMonad m => ExceptT HiError m HiValue
monadFalse = returnBool False 

monadLess :: (HiMonad m, Ord a) => a -> a -> ExceptT HiError m HiValue
monadLess = monadF (<)

monadF :: HiMonad m => (a -> a -> Bool) -> a -> a -> ExceptT HiError m HiValue
monadF f x y = return $ HiValueBool $ f x y

returnNull :: HiMonad m => ExceptT HiError m HiValue
returnNull = return HiValueNull

returnString :: HiMonad m => Text -> ExceptT HiError m HiValue
returnString text = return $ HiValueString text

returnNumber :: HiMonad m => Rational -> ExceptT HiError m HiValue
returnNumber num = return $ HiValueNumber num

returnList :: HiMonad m => Seq HiValue -> ExceptT HiError m HiValue
returnList sq = return $ HiValueList sq

returnBytes :: HiMonad m => BT.ByteString -> ExceptT HiError m HiValue
returnBytes bytes = return $ HiValueBytes bytes

returnBool :: HiMonad m => Bool -> ExceptT HiError m HiValue
returnBool bool = return $ HiValueBool bool

returnDict :: HiMonad m => Map.Map HiValue HiValue -> ExceptT HiError m HiValue
returnDict mp = return $ HiValueDict mp

returnTime :: HiMonad m => UTCTime -> ExceptT HiError m HiValue
returnTime time = return $ HiValueTime time

returnAction :: HiMonad m => HiAction -> ExceptT HiError m HiValue
returnAction action = return $ HiValueAction action

invalidFunction :: HiMonad m => ExceptT HiError m a 
invalidFunction = throwE HiErrorInvalidFunction

invalidArgument :: HiMonad m => ExceptT HiError m a 
invalidArgument = throwE HiErrorInvalidArgument

arityMismatch :: HiMonad m => ExceptT HiError m a
arityMismatch = throwE HiErrorArityMismatch
