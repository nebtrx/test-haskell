{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Main where

import           Lib

data ShowBox = forall s. Show s => SB s

instance Show ShowBox where
    show (SB s) = show s

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]


data Compare a = forall b. Ord b => ASC (a -> b)
               | forall b. Ord b => DESC (a -> b)

data Compare2 a where
    ASC2  :: Ord b => (a -> b) -> Compare2 a
    DESC2 :: Ord b => (a -> b) -> Compare2 a


main :: IO ()
main = someFunc
