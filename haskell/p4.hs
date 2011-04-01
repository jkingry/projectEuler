module P4 ( p4, p4Test ) where

import EulerCommon

threeDigitNumbers = [999,998..100]

products = zipWith (*) threeDigitNumbers threeDigitNumbers

fp4 n = foldr max 0 palindromeNumbers 
	where
		palindromeNumbers = map read palindromes
		palindromes = filter isPalindrome (map show products) 
		products = [ x*y | x <- numbers, y <- numbers ]
		numbers = [b,b-1..e]
		b = (10 ^ n) - 1
		e = (10 ^ (n - 1))

p4 = fp4 3

p4Test = eulerTest "p4" 906609 p4