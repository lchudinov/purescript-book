module Test.MySolutions where

import Prelude
import Data.Person (Person)

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)


-- (Medium) Write a function binomial which finds the coefficient of the xk
-- th term in the polynomial expansion of (1+x)n
-- . This is the same as the number of ways to choose a subset of k
--  elements from a set of n
--  elements. Use the formula n!/k!(n−k)!
-- , where !
--  is the factorial function written earlier. Hint: Use pattern matching to handle corner cases. If it takes a long time to complete or crashes with an error about the call stack, try adding more corner cases.

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n > k = factorial n / ((factorial k) * factorial (n - k))
             | n == k = 1
             | otherwise = 0
             
pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity {address: {city: cityOne}} {address: {city: cityTwo}} = cityOne == cityTwo 