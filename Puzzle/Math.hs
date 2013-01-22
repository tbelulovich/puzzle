module Puzzle.Math where

primes = 2:(filter isPrime [3,5..])
isPrime x = isPrime' x (takeWhile (\y -> (fromIntegral y) <= sqrt (fromIntegral x)) primes)
  where isPrime' x [] = True
        isPrime' x (y:ys) = if (x `mod` y == 0) 
                               then False
                               else isPrime' x ys
                                    
fac 0 = 1
fac n = n * (fac (n-1))