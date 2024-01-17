module Main (main) where

import System.Console.Haskeline
import HW5.Parser (parse)
import HW5.Evaluator(eval)
import HW5.Pretty(prettyValue)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Set (fromList, Set)
import HW5.Action
import Text.Megaparsec hiding (parse)
import Prettyprinter (Pretty(pretty))

permissions :: Set HiPermission
permissions = fromList [AllowRead, AllowTime, AllowWrite]

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just ":q" -> return ()
               Just input -> do
                    case parse input of
                        Left bundle -> outputStrLn $ "Error while parsing expression!\n" ++ show (pretty $ errorBundlePretty bundle) 
                        Right value -> do
                            res <- liftIO $ runHIO (eval value) permissions 
                            case res of
                                Left e  -> outputStrLn $ show e
                                Right x -> outputStrLn $ show $ prettyValue x 
                    loop
