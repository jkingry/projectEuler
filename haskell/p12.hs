module P12 (p12, p12Test) where

import EulerCommon
import Math.Sieve.Factor
-- triNumbers = 0 : zipWith (+) [1..] triNumbers 

triNumbers = map triNumber [1..] 
triNumber n = sum [1..n]

s = sieve $ 2^30

fp12 n list = head $ dropWhile ((<n) . factorCount) list 
    where factorCount n = product $ map ((+1) . snd) (factor s n) 

p12 = return $ fp12 500 triNumbers

p12Test = eulerTest "p12" 76576500 p12



