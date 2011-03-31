import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import EulerCommon

factor 1 = []
factor n = p:(factor nn)
		where
			nn = n `div` p
			p = firstPrime n

firstPrime n = head (filter (\p -> n `rem` p == 0) primes) 

-- http://book.realworldhaskell.org/read/barcode-recognition.html#id631625 
type Run = Int
type RunLength a = [(a, Run)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
	where rle xs = (head xs, length xs)

allFactors n = map (runLength . factor) [2..n]

possiblePrimes n = takeWhile (<n) primes

maxInList p = (foldr max 0) . (map $ (fromMaybe 0) . (lookup p))

maxFactors n = [ (p, (maxInList p af)) | p <- possiblePrimes n] 
	where 
		af = allFactors n

productRun :: Integral a => Integral b => [(a,b)] -> a
productRun = (foldr (*) 1) . (map tm) 
	where tm (p,c) = p ^ c 

p5 n = productRun (maxFactors n)

main = print (p5 20)


















