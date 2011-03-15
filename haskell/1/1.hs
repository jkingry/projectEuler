threeFiveFilter n 
	| (mod n 3) == 0 = n
	| (mod n 5) == 0 = n
        | otherwise = 0

gen f [] = []
gen f (x:xs) = (f x):(gen f xs)


p1 n = sum (gen threeFiveFilter [1..n-1])


main = print (p1 1000)

