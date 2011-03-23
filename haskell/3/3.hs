-- http://www.haskell.org/haskellwiki/Prime_numbers#Generated_Spans.2C_by_List_of_Primes
primes = 2 : oddprimes
oddprimes = 3 : sieve oddprimes 3 0
sieve (p:ps) x k = 
	[n | 
		n <- [x+2,x+4..p*p-2], 
		and [rem n q /= 0 | q <- take k oddprimes]]
	++
	sieve ps (p*p) (k+1)

p3 n = foldr max 0 divisors
	where 
		divisors = filter
			(\d -> rem n d == 0)
			possible
		possible = (takeWhile (< limit) primes)
		limit = (round . sqrt . fromIntegral) n  

main = print (p3 600851475143)
