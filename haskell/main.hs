module Main where

import System.Environment ( getArgs )
import Test.HUnit
import P1
import P2
import P3
import P4
import P5 
import P6
import P7
import P8 
import P9 
import P10 
import P11 
import P12
import P13
import P14
import P15
import P16
import P17
import P18
import P19
import P20
import P21

orderedSolutions = [p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21]
unorderedSolutions = []

solutions = (zip [1..] orderedSolutions) ++ unorderedSolutions
    where ns = length solutions

tests = TestList [p1Test, p2Test,p3Test,p4Test,p5Test,p6Test,p7Test,p8Test,p9Test,p10Test,p11Test,p12Test,p13Test,p14Test,p15Test,p16Test,p17Test,p18Test,p19Test,p20Test,p21Test]

main = do
        args <- getArgs
        if (length args) == 0 
            then do 
                results <- runTestTT tests
                print results
            else 
                print $ runSolution args

runSolution :: [String] -> Integer
runSolution args = func 
    where n = (read $ head args)
          func = case (lookup n solutions) of
            Just f -> f
            Nothing -> error $ "No solution for problem " ++ (show n)


