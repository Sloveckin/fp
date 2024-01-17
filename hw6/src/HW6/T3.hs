module HW6.T3
  ( Config (..)
  , Cell (..)
  , TableConfig (..)
  , CellState (..)
  , Comonad19Grid 
  , Grid (..)
  , simulate
  , gShow'
  ) where

import System.Random (StdGen, mkStdGen, randomR, randoms)
import Data.Grid (Grid (..), neighbours, gWrite)
import Control.Comonad
import Data.ListZipper

data Config = Config
  { probability :: Double
  , incubationPeriod :: Int
  , illnessDuration :: Int
  , immunityDuration :: Int
  } deriving Show

-- | Help data for storing size, iterations and random numbers
data TableConfig = TableConfig 
  { sizeTable :: Int
  , iters :: Int
  , rand1 :: Int
  , rand2 :: Int
  , rand3 :: Int
  }

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int

data Cell = Cell
  { cellState :: CellState
  , cellRand :: StdGen
  }

instance Show Cell where
  show (Cell Healthy _)      = "_"
  show (Cell (Infected _) _) = "i"
  show (Cell (Ill _) _)      = "#"
  show (Cell (Immune _) _)   = "@"

type Comonad19Grid = Grid Cell

-- | Show Grid with size 2 * n - 1
gShow' :: Show a => Grid a -> Int -> String
gShow' grid n = unlines s <> "\n"
  where
    t = toList (unGrid grid) n 
    s = map (`lShow'` n) t

-- | Show ListZipper with size
lShow' :: Show a => ListZipper a -> Int -> String
lShow' (LZ lz a rz) n = l <> show a <> r
  where
    l = concatMap show (reverse $ take n lz)
    r = concatMap show (take n rz)

-- | Counts whether or not it is infected and return new StdGen
inRange :: StdGen -> Double -> (Bool, StdGen)
inRange gen range = let s = randomR (0, 1.0) gen
                    in (fst s <= range, snd s)

-- | Function to determine whether a cell can infect
canInfect :: CellState -> Bool
canInfect (Infected _) = True
canInfect (Ill _)      = True
canInfect _            = False

-- | Get infected or illness neighbour cells
contagiousNeighbors :: Grid Cell -> [Cell]
contagiousNeighbors g = f t
  where
    t = map (\dir -> extract $ dir g) neighbours
    f = filter (canInfect . cellState)

-- | Describes the behavior of a cell
rule :: Config -> Grid Cell -> Cell
rule config grid = case cellState cur of
  Infected n -> ruleForInfected config n newGen 
  Immune n   -> ruleForImmune newGen n
  Healthy    -> ruleForHealthy config newGen infected 
  Ill n      -> ruleForIll config n newGen 
  where
    cur       = extract grid
    contagArr = contagiousNeighbors grid
    oldGen    = cellRand cur
    prob      = probability config

    -- I'm so sorry for this trash...
    -- I was trying replace this by last $ take n $ iterate ...
    -- We wanna change StdGen on each iteration
    (infected, newGen) = foldl (
      \(b, g) _ -> 
      let (newB, newG) = inRange g prob 
      in (b || newB, newG)) 
      (False, oldGen) contagArr 


-- | Behaviour of infected cell
ruleForInfected :: Config -> Int -> StdGen  -> Cell
ruleForInfected config n = Cell (infectedTick n) 
  where
  infectedTick 0 = Ill $ incubationPeriod config
  infectedTick x = Infected $ pred x 


-- | Behaviour of ill cell
ruleForIll :: Config -> Int -> StdGen -> Cell
ruleForIll config n = Cell (illTick n)
  where
    illTick 0 = Immune $ immunityDuration config
    illTick x = Ill $ pred x

-- | Behaviour of healty cell
ruleForHealthy :: Config -> StdGen -> Bool -> Cell
ruleForHealthy config gen True = Cell (Infected $ incubationPeriod config) gen 
ruleForHealthy _ gen False     = Cell Healthy gen 

-- | Behaviour of immune cell
ruleForImmune :: StdGen -> Int -> Cell
ruleForImmune gen 0 = Cell Healthy gen 
ruleForImmune gen n = Cell (Immune $ pred n) gen 


-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
--
-- This function may take additional parameters (e.g. initial seed for random).
simulate :: Config -> TableConfig -> [Comonad19Grid]
simulate config tableConfig = grids 
  where
    iter  = iters tableConfig
    cell  = Cell (Infected $ incubationPeriod config) (mkStdGen 0)
    grid  = gWrite cell (createGrid tableConfig)
    grids = take iter $ iterate (evolve config) grid

-- | Evolve
evolve :: Config -> Grid Cell -> Grid Cell
evolve config = extend (rule config)

-- | Create initial grid
createGrid :: TableConfig -> Comonad19Grid
createGrid tableConfig = cellGrid 
  where
    r1       = rand1 tableConfig 
    r2       = rand2 tableConfig
    r3       = rand3 tableConfig
    lz       = LZ (randoms $ mkStdGen r1) r2 (randoms $ mkStdGen r3) 
    seeds    = Grid $ genericMove (fmap (* r3)) (fmap ( * r1)) lz
    cellGrid = fmap (Cell  Healthy . mkStdGen) seeds 
