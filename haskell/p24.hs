module P24 (p24,p24Test) where

import EulerCommon
import Data.List

fp24 :: Integer -> Integer
fp24 n = read $ map (head.show) $ (sort $ permutations [0..9]) `genericIndex` (n - 1)

p24 = return $ fp24 1000000

p24Test = eulerTest "p24" 2783915460 p24
