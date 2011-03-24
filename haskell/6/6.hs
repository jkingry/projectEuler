p6 n = squareOfSum - sumOfSquares 
	where
		sumOfSquares = foldr (+) 0 (map (^2) [1..n])
		squareOfSum = sum ^ 2 
		sum = foldr (+) 0 [1.. n]


