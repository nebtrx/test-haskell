module Lib where

import           Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                                     updateAction, updateFreq)
import           Control.Concurrent
import           Control.Monad
import           Data.Time


someFunc :: IO ()
someFunc = putStrLn "someFunc"

fn :: IO ()
fn = do
    getTime <- mkAutoUpdate defaultUpdateSettings
        { updateAction = getCurrentTime
        , updateFreq = 1000 -- The default frequency, once per second
        }

    replicateM_ 10000 $ do
        currentTime <- getTime
        print . show $ currentTime
        threadDelay 2000000
    return ()
