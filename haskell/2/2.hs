
-- fib 1 = 1
-- fib 2 = 2
-- fib n = fib (n - 1) + fib (n - 2)



fibList [] = [1]
fibList [1] = [2,1]
fibList (a:b:xs) = (a+b) : a : b : xs

fib 0 list = list
fib n list = fib (n - 1) (fibList list)

evenFilter [] = []
evenFilter (x:xs) = if (mod x 2) == 0 
	then x : evenFilter(xs)
	else evenFilter(xs)


main = print (sum (fib 10))