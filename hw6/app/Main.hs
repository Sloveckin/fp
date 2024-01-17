module Main (main)
where

import Options.Applicative
import System.Random
import HW6.T3
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Exception 

data Input = Input
  { prob :: Double
  , incub :: Int
  , ill :: Int
  , immun :: Int
  , gridSize :: Int
  , iterations :: Int
  } deriving Show

newtype ArgumentException = ArgumentException String
  deriving Show

instance Exception ArgumentException

probStr :: String
probStr = "Probability of disease transmission"

incubStr :: String
incubStr = "Incubation period of disease"

illStr :: String
illStr = "Day of illnes"

immunStr :: String
immunStr = "Day immunity exists"

gridSizeStr :: String
gridSizeStr = "Grid size"

iterationsStr :: String
iterationsStr = "Number of iterations"

-- | Delay in drawing the next grid
printDelay :: Int
printDelay = 100000

inputParser :: Parser Input
inputParser = Input
  <$> option auto (long "prob" <> metavar "PROB" <> help probStr)
  <*> option auto (long "incub" <> metavar "INCUB" <> help incubStr)
  <*> option auto (long "ill" <> metavar "ILL" <> help illStr)
  <*> option auto (long "immun" <> metavar "IMMUN" <> help immunStr)
  <*> option auto (long "grid-size" <> metavar "GRIDSIZE" <> help gridSizeStr)
  <*> option auto (long "iterations" <> metavar "ITERATIONS" <> help iterationsStr)

main :: IO ()
main = do
  input <- execParser $ info (inputParser <**> helper) fullDesc
  r <- try $ checkConsoleArgs input
  case r of 
    Left (ArgumentException x) -> putStrLn x 
    Right _ -> do 
      let config = Config (prob input) (incub input) (ill input) (immun input)
      tableConfig <- getTableConfig input
      let grids = simulate config tableConfig
      printGrids (sizeTable tableConfig) grids 

-- | Get three random int
randThree :: IO (Int, Int, Int)
randThree =
  let range = (0 :: Int, 100 :: Int)
  in do
    r1 <- getStdRandom $ randomR range
    r2 <- getStdRandom $ randomR range
    r3 <- getStdRandom $ randomR range
    return (r1, r2, r3)

-- | Print each grid
printGrids :: Int -> [Comonad19Grid] -> IO ()
printGrids size = mapM_ (\grid -> do 
                                putStrLn $ gShow' grid (size - 1) 
                                threadDelay printDelay)

-- | Get Table config
getTableConfig :: Input -> IO TableConfig
getTableConfig input = do
    (r1, r2, r3) <- randThree
    let size = gridSize input
    let itr = iterations input
    return $ TableConfig size itr r1 r2 r3

-- | Check each arguments
checkConsoleArgs :: Input -> IO ()
checkConsoleArgs input = do
  let pr = prob input
  when (pr > 1 || pr <= 0) (throwIO $ ArgumentException "Probability must be > 0 and <= 1.") 
  check incubStr (incub input)
  check illStr (ill input)
  check immunStr (immun input)
  check gridSizeStr (gridSize input)
  check iterationsStr (iterations input)

-- | Help function for checking agruments
check :: String -> Int -> IO ()
check text val 
  | val <= 0  = throwIO $ ArgumentException $ text <> " must be > 0."
  | otherwise = return () 