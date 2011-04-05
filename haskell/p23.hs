module P23 (p23, p23Test) where

import EulerCommon
import Data.List
import Data.Array

pairs :: [a] -> [[a]]
pairs [] = []
pairs a@(x:xs) = (map ((x:).(:[])) a) ++ (pairs xs)

isAbd n = n < (sum $ strictFactors n)

fp23 n = sum $ filter (not . xHasAbdSum) [1..n]
	where		
		abdFlag = map isAbd [1..n]
		abdFlag_array = listArray (1,n) $ abdFlag 
		isAbd_array x = abdFlag_array ! x
		abds = filter isAbd_array [1..n]
		xMinusAbd x = map (x-) $ takeWhile (<= x `div` 2) abds
		xHasAbdSum x = any isAbd_array $ xMinusAbd x
	
p23 = return $ fp23 28124

p23Test = eulerTest "p23" 4179871 p23