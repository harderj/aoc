module Common where

infixr 1 >.>
(f >.> g) x = g (f x)
