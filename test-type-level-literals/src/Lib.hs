{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
module Lib where

import           Control.Monad (forM_, when)
import           Data.Maybe    (isJust)
import           Data.Proxy
import           GHC.TypeLits  (KnownSymbol, SomeSymbol (..), Symbol, natVal,
                                sameSymbol, someSymbolVal, symbolVal)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Label (l :: Symbol) = Get

class Has t l v| t l -> v where
  from :: t -> Label l -> v

data Point = Point Int Int deriving Show

instance Has Point "x" Int where
    from (Point x _) _ = x
instance Has Point "y" Int where
    from (Point _ y) _ = y
instance Has Point "n" Int where
    from _ _ = 5


example = from (Point 1 2) (Get :: Label "y")
exampleN = from (Point 1 2) (Get :: Label "n")
-- exampleU = from (Point 1 2) (Get :: Label "u")
exampleNV = natVal (Proxy :: Proxy 2)
exampleSV = symbolVal (Proxy :: Proxy "nananananana batman")



-- | An existential box to place anything that is a Handler and KnownSymbol.
data SomeHandler
    = forall h. (KnownSymbol h, Handler h) => SomeHandler (Proxy h)

class Handler h where
    -- We need to pass the proxy in here because a class has to work on a
    -- value that mentions at least one type variable.
    handleIt :: Proxy h -> IO ()

-- | The type just goes inline as a literal.
instance Handler "dance" where
    handleIt _ = putStrLn "*   *\n \\o/\n _|\n   \\"

same :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
same a b = isJust (sameSymbol a b)

handler :: SomeHandler
handler = SomeHandler (Proxy :: Proxy "dance")

-- extractProxy :: forall h.(KnownSymbol h, Handler h) => SomeHandler -> Proxy h
-- -- extractProxy :: forall h.(KnownSymbol h, Handler h) => SomeHandler -> Proxy h
-- extractProxy (SomeHandler proxy) = proxy


handleCommand :: String -> IO ()
handleCommand str = do
    -- let proxy = (_ handler) :: Proxy h)
    let proxy = Proxy :: Proxy "dance"

    case someSymbolVal str of
        SomeSymbol user_proxy -> when (same user_proxy proxy) (handleIt proxy)

    -- where
-- NOTE: Not sure why this is not working
-- extractProxy :: forall h.(KnownSymbol h, Handler h) => SomeHandler -> Proxy h
-- extractProxy (SomeHandler proxy) = proxy

-- extractProxy = \(SomeHandler proxy) -> proxy

handlers :: [SomeHandler]
handlers = [handler]

handleCommand1 :: String -> IO ()
handleCommand1 str =
    -- A case statement is needed to extract the existential here, or GHC's
    -- brain explodes.
    case someSymbolVal str of
        SomeSymbol user_proxy ->
            forM_ handlers $ \(SomeHandler proxy) ->
                when (same user_proxy proxy) (handleIt proxy)
