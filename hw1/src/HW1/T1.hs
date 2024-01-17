module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay Monday = Tuesday 
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday 
nextDay Sunday = Monday 



afterDays :: Natural -> Day -> Day
afterDays n day = if n == 0
                  then day 
                  else afterDays (n - 1) (nextDay day) 


isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

daysToParty :: Day -> Natural
daysToParty = res 
  where 
    goToFriday n Friday = n 
    goToFriday n day = goToFriday (n + 1) (nextDay day)
    res = goToFriday 0

