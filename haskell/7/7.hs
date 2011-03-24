-- http://www.haskell.org/haskellwiki/Prime_numbers#Generated_Spans.2C_by_List_of_Primes
primes = 2 : oddprimes
oddprimes = 3 : sieve oddprimes 3 0
sieve (p:ps) x k = 
	[n | 
		n <- [x+2,x+4..p*p-2], 
		and [rem n q /= 0 | q <- take k oddprimes]]
	++
	sieve ps (p*p) (k+1)

p7 n = last (take n primes)

main = print (p7 10001)
