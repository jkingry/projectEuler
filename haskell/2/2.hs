fib 1 = 1
fib 2 = 2
fib n = fib (n - 1) + fib (n - 2)

p2a n limit = if (fib n) < limit
		then if mod (fib n) 2 == 0
			then (fib n) + p2a (n + 1) limit 
			else p2a (n + 1) limit
		else 0

main = print (p2a 1 4000000)