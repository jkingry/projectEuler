module P12 (p12, p12Test) where

import EulerCommon
import Data.List

triNumbers = scanl1 (+) [1..]

fp12 n list = head $ dropWhile ((<n) . factorCount) list 
    where factorCount n = product $ map ((+1) . length) (group (primeFactors n))

p12 = return $ fp12 500 triNumbers

p12Test = eulerTest "p12" 76576500 p12



