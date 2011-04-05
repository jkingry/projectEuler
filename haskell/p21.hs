module P21 (p21, p21Test) where

import EulerCommon

import Data.List
   
fp21 n = sum $ filter isAmicable [2..n]

isAmicable n = n /= n' && n == (sf n') 
    where sf = (sum . strictFactors) 
          n' = sf n

p21 = return $ fp21 10000

p21Test = eulerTest "p21" 31626 p21
