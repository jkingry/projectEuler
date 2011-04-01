module EulerCommon
where

import Data.List
import Test.HUnit

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- http://www.haskell.org/haskellwiki/Prime_numbers#Generated_Spans.2C_by_List_of_Primes
primes = 2 : oddprimes
oddprimes = 3 : sieve oddprimes 3 0
sieve (p:ps) x k = 
	[n | 
		n <- [x+2,x+4..p*p-2], 
		and [rem n q /= 0 | q <- take k oddprimes]]
	++
	sieve ps (p*p) (k+1)

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs) | x == last xs = isPalindrome (init xs)
		            | otherwise = False

subs :: Int -> [a] -> [[a]]
subs n = map (take n) . filter ((>=n) . length) . tails

eulerTest s n f = TestCase (assertEqual s n f)
