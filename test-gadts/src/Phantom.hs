module Phantom() where



data Expr a = Int a
            | B Bool
            | Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Eq (Expr a) (Expr a)

-- Add :: Expr a -> Expr a -> Expr a

add :: Expr Int -> Expr Int -> Expr Int
add = Add

