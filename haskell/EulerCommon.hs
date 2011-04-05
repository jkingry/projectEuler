module EulerCommon
(
	fibs,
	isPalindrome,
	subs,
	eulerTest,
	primes,
	primeFactors,
	readArray,
	numberToDigits
)
where

import Data.List
import Test.HUnit

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs) | x == last xs = isPalindrome (init xs)
		            | otherwise = False
subs :: Int -> [a] -> [[a]]
subs n = map (take n) . filter ((>=n) . length) . tails

eulerTest :: String -> Integer -> IO Integer -> Test
eulerTest s n f = s ~: f >>= assertEqual s n

readArray :: String -> [[Integer]]
readArray x = map ((map read) . words) (lines x)

numberToDigits :: Integer -> [Integer]
numberToDigits n = map (read.(:[])) (show n)


-- http://www.haskell.org/haskellwiki/Euler_problems/1_to_10#Problem_3
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps 

