module P7 ( p7, p7Test ) where

import EulerCommon 

fp7 n = last (take n primes)

p7 = return $ fp7 10001

p7Test = eulerTest "p7" 104743 p7