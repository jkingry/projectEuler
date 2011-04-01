module P9 ( p9, p9Test ) where

import EulerCommon

bruteForceTriplet = [ (a,b,c) | 
	c <- [1..998],
	a <- [1..c],
	b <- [1..a],
	a^2 + b^2 == c^2,
	a + b + c == 1000]

{-
Simplification of equations

a^2 + b^2 = c^2
a + b + c = 1000

c = 1000 - (a+b)

a^2 + b^2 = ((1000 - (a+b))^2

a^2 + b^2 = 1000000 - 2000a - 2000b + 2ab + a^2 + b^2

0 = 1000000 - 2000a - 2000b + 2ab

500000 = 1000a + 1000b - ab 
-}

solution = [ (a,b) | 
	a <- [1..998],
	b <- [1..a],
	500000 == 1000*a + 1000*b - a*b]

(a,b) = head solution
c = 1000 - (a+b)

p9 = a*b*c

p9Test = eulerTest "p9" 31875000 p9