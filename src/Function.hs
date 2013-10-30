module Function (($$)) where

infixr 0 $$
($$) :: a -> (a -> b) -> b
($$) = flip ($)
