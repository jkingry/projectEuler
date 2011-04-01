module P14 (p14, p14Test) where

import EulerCommon
import Data.List
import qualified Data.Map as M
import Text.Printf
import Control.Exception
import System.CPUTime

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

ctz :: Integer -> Integer
ctz 1 = 1
ctz n
    | even n = n `div` 2
    | otherwise =  3*n + 1

lctz :: M.Map Integer Integer -> Integer -> (M.Map Integer Integer, Integer)
lctz m 1 = (M.empty, 1)  
lctz m n = case (M.lookup n m) of
        Just i -> (m, i)
        otherwise -> (curMap, curLength)  
    where next = lctz m (ctz n) 
          curLength = 1 + (snd next)
          curMap = M.insert n curLength (fst next)

maxLctz n = foldl maxTuple (0, 0, M.empty) [1,3..n]
    where
        maxTuple (pn,prevLength,prevMap) cn = if curLength > prevLength then (cn, curLength, curMap) else (pn, prevLength, curMap)  
            where 
                (curMap, curLength) = lctz prevMap cn

fp14 n = (maxN, maxLength) 
    where
        (maxN, maxLength,_) = maxLctz n

p14 = maxN
    where (maxN, _) = fp14 1000000 

p14Test = eulerTest "p14" 837799 p14
