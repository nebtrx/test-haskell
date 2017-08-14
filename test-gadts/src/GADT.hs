{-# LANGUAGE GADTs #-}

module GADT ( Expr(..)
            , eval
            ) where

data Expr a where
    I :: Int -> Expr Int
    B :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mult :: Expr Int -> Expr Int -> Expr Int
    Eq :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (I i)      =  i
eval (B b)      =  b
eval (Add l r)  = eval l + eval r
eval (Mult l r) = eval l * eval r
eval (Eq l r)   = eval l == eval r
