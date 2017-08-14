module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Expr = I Int
          | Add Expr Expr
          | Mult Expr Expr



eval :: Expr -> Int
eval (I i)      = i
eval (Add l r)  = eval l + eval r
eval (Mult l r) = eval l * eval r
