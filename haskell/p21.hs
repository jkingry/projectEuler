module P21 (p21, p21Test) where

import EulerCommon

import Data.List
import Math.Sieve.Factor
import qualified Data.Set as Set

q = sieve 32000

distinct = Set.toList . Set.fromList

factors n = map product $ distinct $ subsequences $ concat $ map r $ factor q n
    where r (p,n) = genericReplicate n p

strictFactors n = filter (<n) $ factors n
   
fp21 n = sum $ filter isAmicable [2..n]

isAmicable n = n /= n' && n == (sf n') 
    where sf = (sum . strictFactors) 
          n' = sf n

p21 = return $ fp21 10000

p21Test = eulerTest "p21" 31626 p21
