primes = 2 : oddprimes
oddprimes = 3 : sieve oddprimes 3 0
sieve (p:ps) x k = 
	[n | 
		n <- [x+2,x+4..p*p-2], 
		and [rem n q /= 0 | q <- take k oddprimes]]
	++
	sieve ps (p*p) (k+1)

p10 = sum $ takeWhile (<2000000): primes

main = print p10