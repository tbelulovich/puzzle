module Puzzle.Math 
       (module Puzzle.Math
       )
       where

import Data.List                                    

-- | Factorial
fac :: (Eq a, Num a) => a -> a
fac 0 = 1
fac n = n * (fac (n-1))

newtype Poly a = Poly {coeffs :: [a]} deriving (Show, Eq)

instance Functor Poly where
  fmap f (Poly g) = Poly (map f g)

instance (Num a) => Num (Poly a) where
  fromInteger n = Poly [fromInteger n]
  (Poly []) + x = x
  x + (Poly []) = x
  (Poly (x:xs)) + (Poly (y:ys)) = Poly ((x+y):(coeffs $ (Poly xs) + (Poly ys)))
  negate = fmap negate
  (Poly []) * _ = (Poly [])
  _ * (Poly []) = (Poly [])
  (Poly (x:xs)) * g = a + b where
    a = fmap (* x) g
    b = Poly (0:(coeffs $ (Poly xs) * g))
  signum = undefined
  abs = undefined

fromBase :: Num a => 
            a -- ^ base 
            -> [a] -- ^ digits
            -> a -- ^ digits taken in base 
fromBase b digits = 
  foldl (\x y -> b * x + y) 0 digits

-- | Strict version of fromBase
fromBase' :: Num a => a -> [a] -> a
fromBase' b digits =
  foldl' (\x y -> b * x + y) 0 digits

-- | Extract digits of an integral type in a given number base
toBase :: Integral a 
          => a -- ^ base
          -> a -- ^ number to be converted
          -> [a] -- ^ digits
toBase b = 
  reverse . 
  map (`mod` b) .
  takeWhile (> 0) .
  iterate (`div` b)
