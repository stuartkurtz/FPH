-- author: Stuart A. Kurtz

module Collatz where

-- The 3n+1 function of number theory.

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

--  On input n, return the minimal number of iterations of `collatz`
--  needed to map n to 1.

csteps :: Integer -> Integer
csteps n
  | n <= 0 = error "csteps: called with non-postive argument"
  | n == 1 = 0
  | otherwise = 1 + csteps (collatz n)
