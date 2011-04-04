module P20 (p20,p20Test) where

import EulerCommon

f :: Integer -> Integer
f x = product [1..x]

fp20 :: Integer -> Integer
fp20 x = sum $ numberToDigits $ f x 

p20 = fp20 100

p20Test = eulerTest "p20" 648 p20 
