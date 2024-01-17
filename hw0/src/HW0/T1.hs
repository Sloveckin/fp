{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib x = case x of 
      Left a       -> (Left a, Left a) 
      Right (b, c) -> (Right b, Right c) 

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g)= Iso g f 

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f  

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c))) 

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso leftToRight rightToLeft  

leftToRight :: Either a (Either b c) -> Either (Either a b) c
leftToRight x = case x of 
  Left a              -> Left $ Left a 
  Right anotherEither -> case anotherEither of 
                            Left  b  -> Left $ Right b 
                            Right c  -> Right c

rightToLeft :: Either (Either a b) c -> Either a (Either b c)
rightToLeft x = case x of 
  Right            c -> Right $ Right c
  Left anotherEither -> case anotherEither of
                          Left  a -> Left a
                          Right b -> Right $ Left b