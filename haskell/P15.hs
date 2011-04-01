module P15 (p15,p15Test) where

import EulerCommon

f x = product [1..x]

fp15 :: Integer -> Integer
fp15 n = (f (n*2)) `div` ((f n) * (f n)) 

p15 = fp15 20

p15Test = eulerTest "p15" 137846528820 p15
