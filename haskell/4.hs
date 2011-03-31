import EulerCommon

threeDigitNumbers = [999,998..100]

products = zipWith (*) threeDigitNumbers threeDigitNumbers

p4 :: Int -> Int 
p4 n = foldr max 0 palindromeNumbers 
	where
		palindromeNumbers = map read palindromes
		palindromes = filter isPalindrome (map show products) 
		products = [ x*y | x <- numbers, y <- numbers ]
		numbers = [b,b-1..e]
		b = (10 ^ n) - 1
		e = (10 ^ (n - 1))

main = print (p4 3)
