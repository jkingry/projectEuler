module P1 ( p1, p1Test ) where

import EulerCommon
import Test.HUnit

fp1 n = sum (filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..n-1])

p1 = fp1 1000

p1Test = eulerTest "P1" 233168 p1