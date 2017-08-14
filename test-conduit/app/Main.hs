module Main where

import           Data.Conduit
import qualified Lib          as L
import qualified Primitives   as P

main :: IO ()
main = do
    L.source =$ L.newConduit $$ L.conduit =$ L.sink
    P.source $$ P.conduit =$ P.sink
    -- alternatively, with the same meaning
    -- source $= conduit $$ sink

