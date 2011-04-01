module P10 ( p10, p10Test ) where

import EulerCommon

p10 = sum $ takeWhile (<2000000) primes

p10Test = eulerTest "p10" 142913828922 p10