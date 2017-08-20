{-# LANGUAGE TemplateHaskell #-}


module Main where

import           Language.Haskell.TH
import           Lib


data MyData = MyData
            { foo :: String
            , bar :: Int
            }

listFields ''MyData

main :: IO ()
main = print $ MyData { foo = "bar", bar = 5 }
