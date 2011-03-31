import EulerCommon

p3 n = foldr max 0 divisors
	where 
		divisors = filter
			(\d -> rem n d == 0)
			possible
		possible = (takeWhile (< limit) primes)
		limit = (round . sqrt . fromIntegral) n  

main = print (p3 600851475143)
