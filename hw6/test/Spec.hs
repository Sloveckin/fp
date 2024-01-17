import Test.HUnit

import HW6.T1
import Control.Concurrent.Classy
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import System.Random (mkStdGen, Random (randoms))
import Control.Monad (forM_)

getTest :: CHT (STM IO) Int Int -> Int -> Maybe Int -> Test
getTest mp key expected = TestCase $ getCHT key mp >>= assertEqual "" expected

countOfTests :: Int 
countOfTests = 50

getPutTests :: IO ()
getPutTests = do
    mp <- newCHT
    putter mp
    getter mp


sizeTest :: Int -> Int -> Test
sizeTest res s = TestCase $ assertEqual "" res s

sizeTests :: IO ()
sizeTests = do
    mp <- newCHT
    putCHT (1 :: Int) (10 :: Int) mp
    s1 <- sizeCHT mp
    let t1 = TestLabel "size-test-1" (sizeTest s1 1)
    putCHT 2 30 mp
    s2 <- sizeCHT mp
    let t2 = TestLabel "size-test-2" (sizeTest s2 2)
    putCHT 2 10 mp
    s3 <- sizeCHT mp
    let t3 = TestLabel "size-test-3" (sizeTest s3 2)
    _ <- runTestTT $ TestList [t1, t2, t3]
    return ()

testResize :: IO ()
testResize = do
  let genKey = mkStdGen 100
  let genValue = mkStdGen 231
  let keys = take countOfTests (randoms genKey :: [Int])
  let values = take countOfTests (randoms genValue :: [Int])
  mp <- newCHT
  let pairs = zip keys values
  forM_ pairs (\(k, v) -> do putCHT (k :: Int) (v :: Int) mp) 
  _ <- runTestTT $ TestList $ map (\(k, v) -> TestLabel "" (getTest mp k (Just v))) pairs
  return () 


putter :: CHT (STM IO) Int Int -> IO ()
putter mp = do
  Control.Concurrent.threadDelay 10000
  putCHT 1 10 mp
  putCHT 2 15 mp
  putCHT 3 25 mp
  return ()

getter :: CHT (STM IO) Int Int -> IO ()
getter mp = do
  Control.Concurrent.threadDelay 20000
  let list = TestList [TestLabel "get-put-test-1" (getTest mp 1 (Just 10))
                         , TestLabel "get-put-test-2" (getTest mp 2 (Just 15))
                         , TestLabel "get-put-test-3" (getTest mp 3 (Just 25))
                         , TestLabel "get-put-test-4" (getTest mp 19 Nothing)]
  _ <- runTestTT list
  return ()

main :: IO ()
main = do
  testsInOneThread
  testInTwoThread

testsInOneThread :: IO ()
testsInOneThread = do
  sizeTests
  getPutTests
  testResize
  return ()

testInTwoThread :: IO ()
testInTwoThread = do
  mp <- newCHT
  _ <- concurrently (putter mp) (getter mp)
  return ()