p1 n = p1a (n - 1)

p1a n = if n == 0
	then 0
	else if (mod n 3) == 0 || (mod n 5) == 0
		then n + p1a (n - 1)
		else p1a (n - 1)

main = print (p1 1000)

