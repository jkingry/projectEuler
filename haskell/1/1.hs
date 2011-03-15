threeFiveFilter n 
	| (mod n 3) == 0 = n
	| (mod n 5) == 0 = n
        | otherwise = 0

p1 n = sum (map threeFiveFilter [1..n-1])


main = print (p1 1000)

