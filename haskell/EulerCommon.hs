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
import Data.Numbers.Primes

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

