module P2 ( p2, p2Test ) where

import EulerCommon

fp2 limit = sum (filter even (takeWhile (<limit) fibs))

p2 = fp2 4000000

p2Test = eulerTest "p2" 4613732 p2