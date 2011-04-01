module P6 ( p6, p6Test ) where

import EulerCommon

fp6 n = squareOfSum - sumOfSquares 
	where
		sumOfSquares = foldr (+) 0 (map (^2) [1..n])
		squareOfSum = sum ^ 2 
		sum = foldr (+) 0 [1.. n]

p6 = fp6 100

p6Test = eulerTest "p6" 25164150 p6