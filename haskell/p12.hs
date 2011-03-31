module P12 (p12, p12Test) where

import EulerCommon
import Math.Sieve.Factor
-- triNumbers = 0 : zipWith (+) [1..] triNumbers 

triNumbers = map triNumber [1..] 
triNumber n = sum [1..n]

fp12 n list = head $ dropWhile ((<n) . factorCount) list 
    where s = sieve $ 2^30
          factorCount n = product $ map ((+1) . snd) (factor s n) 

p12 = fp12 500 triNumbers

p12Test = eulerTest "p12" 76576500 p12


