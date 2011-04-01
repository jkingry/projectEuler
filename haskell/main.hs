module Main ( main ) where

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

solutions = [p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11]
tests = TestList [p1Test, p2Test,p3Test,p4Test,p5Test,p6Test,p7Test,p8Test,p9Test,p10Test,p11Test]

main = do
		args <- getArgs
		if (length args) == 0 
			then do 
				results <- runTestTT tests
				print results
			else print (solution args)
	where
		solutionNumber args = (read $ head args) - 1
		solution args = solutions !! (solutionNumber args) 
