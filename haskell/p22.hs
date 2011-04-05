module P22 (p22, p22Test) where

import EulerCommon
import Data.List
import Data.Char (ord)

ltrim _ [] = []
ltrim c a@(x:xs) 
    | x `elem` c = ltrim c xs
    | otherwise = a

split _ [] = []
split c s = a:(split c b) 
    where
        (a,b) = break (`elem`c) $ ltrim c s

alphaScore :: String -> Integer
alphaScore s = sum $ map ((+a).fromIntegral.ord) s 
    where a = -1 * ((fromIntegral.ord) 'A')+1 

fp22 :: String -> Integer
fp22 s = sum $ zipWith (*) [1..] scores 
    where names = tail $ sort $ split ",\"" s
          scores = map alphaScore names

p22 :: IO Integer          
p22 = do
    input <- readFile "..\\names.txt"
    return (fp22 input)

p22Test = eulerTest "p22" 871198282 p22