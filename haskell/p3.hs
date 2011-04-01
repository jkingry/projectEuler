module P3 ( p3, p3Test ) where

import EulerCommon

fp3 n = foldr max 0 divisors
	where 
		divisors = filter
			(\d -> rem n d == 0)
			possible
		possible = (takeWhile (< limit) primes)
		limit = (round . sqrt . fromIntegral) n  

p3 = fp3 600851475143

p3Test = eulerTest "p3" 6857 p3