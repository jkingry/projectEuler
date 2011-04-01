module P17 (p17,p17Test) where

import EulerCommon
import Data.List

smallWords = ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
tenWords = ["twenty", "thirty","forty","fifty","sixty","seventy","eighty","ninety"]

singles = zip [1..length smallWords] smallWords 
tens = zip [20,30..90] tenWords 

nlook = singles ++ tens ++ [(1000,"one thousand")]

nand 0 = []
nand n = " and " ++ nwords n
nhyphen 0 = []
nhyphen n = "-" ++ nwords n

nwords n = case (lookup n nlook) of 
            Just w -> w
            Nothing -> if n >= 100 
                then (nwords hundred) ++ " hundred" ++ nand (n `rem` 100) 
                else if n > 20 
                    then (nwords ten) ++ nhyphen (n `rem` 10) 
                    else error "wtf"
    where
        hundred = n `div` 100
        ten = 10 * (n `div` 10)

nwordLength n = genericLength $ filter validChar (nwords n) 
    where validChar = (flip elem) ['a'..'z']      

fp17 n = sum $ map nwordLength [1..n]

p17 = fp17 1000

p17Test = eulerTest "p17" 21124 p17
