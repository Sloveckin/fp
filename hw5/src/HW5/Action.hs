{-# LANGUAGE DerivingVia #-}

module HW5.Action (
    HIO (..)
    , HiPermission (..)
    , PermissionException (..)
) where

import Data.Set as ST
import Control.Exception (Exception)
import HW5.Base (HiMonad (..), HiAction (..), HiValue (..))
import Control.Monad.Trans.Reader
import Data.String (fromString)
import System.Directory (getCurrentDirectory, setCurrentDirectory, createDirectory, doesFileExist, listDirectory, doesDirectoryExist)
import Control.Exception.Base (throw)
import Data.ByteString as BT
import qualified Data.Text.Encoding as T
import Data.Text as TT
import Data.Sequence as SQ
import Data.Time (getCurrentTime)
import System.Random (getStdRandom, randomR)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

newtype PermissionException = PermissionRequired HiPermission
    deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
    deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)


instance HiMonad HIO where
    runAction (HiActionChDir path)       = readHIO $ setCurrentDirectory path
    runAction (HiActionMkDir dir)        = writeHIO $ createDirectory dir
    runAction (HiActionWrite path bytes) = writeHIO $ BT.writeFile path bytes
    runAction (HiActionRead path)        = tryReadFileOrDirectory path 
    runAction HiActionCwd                = cwdHIO 
    runAction HiActionNow                = timeHIO 
    runAction (HiActionEcho text)        = echoHIO text 
    runAction (HiActionRand a b)         = HIO $ \_ -> do
                                            randValue <- getStdRandom $ randomR (a, b)
                                            return $ HiValueNumber $ toRational randValue 

readHIO :: IO () -> HIO HiValue
readHIO = readOrWriteHIO AllowRead noPermissionRead

writeHIO :: IO () -> HIO HiValue
writeHIO = readOrWriteHIO AllowWrite noPermissionWrite

readOrWriteHIO :: HiPermission -> IO HiValue -> IO () -> HIO HiValue
readOrWriteHIO p err fun = baseHIO p (HiValueNull <$ fun) err

timeHIO :: HIO HiValue
timeHIO = baseHIO AllowTime (fmap HiValueTime getCurrentTime) noPermissionTime 

cwdHIO :: HIO HiValue
cwdHIO = baseHIO AllowRead (fmap (HiValueString . fromString) getCurrentDirectory) noPermissionRead 

tryReadFileOrDirectory :: FilePath -> HIO HiValue
tryReadFileOrDirectory path = baseHIO AllowRead (readFileOrDirectory path) noPermissionRead 

echoHIO :: Text -> HIO HiValue
echoHIO text = baseHIO AllowWrite (do Prelude.putStrLn $ TT.unpack text; return HiValueNull) noPermissionWrite

baseHIO :: HiPermission -> IO HiValue -> IO HiValue -> HIO HiValue
baseHIO perm fun err = HIO $ \perms ->
        if perm `Prelude.elem` perms
        then fun 
        else err  

noPermissionWrite :: IO HiValue
noPermissionWrite = throw $ PermissionRequired AllowWrite

noPermissionRead :: IO HiValue
noPermissionRead = throw $ PermissionRequired AllowRead

noPermissionTime :: IO HiValue
noPermissionTime = throw $ PermissionRequired AllowTime

readFileOrDirectory :: FilePath -> IO HiValue 
readFileOrDirectory path = do 
    isFile <- doesFileExist path
    if isFile
    then do
        bytes <- BT.readFile path
        case T.decodeUtf8' bytes of
            Left _        -> return $ HiValueBytes bytes
            Right content -> return $ HiValueString content
        else do
            isDir <- doesDirectoryExist path
            if isDir then do
                files <- listDirectory path
                return $ HiValueList $ SQ.fromList $ Prelude.map (HiValueString . TT.pack) files
            else return HiValueNull 