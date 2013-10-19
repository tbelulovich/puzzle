module Puzzle.Math 
       ( module Math.NumberTheory.Primes 
       , fac 
       ) where

import Math.NumberTheory.Primes
                                    
fac 0 = 1
fac n = n * (fac (n-1))