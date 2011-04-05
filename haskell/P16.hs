module P16 (p16,p16Test) where

import EulerCommon

p16 :: IO Integer
p16 = return $ sum $ numberToDigits (2 ^ 1000)

p16Test = eulerTest "p16" 1366 p16


