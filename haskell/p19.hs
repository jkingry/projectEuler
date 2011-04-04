module P19 where

import EulerCommon

import Data.Time.Calendar

dayOfWeek day = diff `mod` 7 
	where  
		epoch = fromGregorian 1900 1 1
		diff = diffDays day epoch

isSunday day = (dayOfWeek day) == 6

months from to 
   | from < to = from:(months (addGregorianMonthsClip 1 from) to)
   | otherwise = []	

fp19 :: Day -> Day -> Integer
fp19 from to = (fromIntegral . length) $ filter isSunday $ months from to 

p19 :: Integer
p19 = fp19 (fromGregorian 1901 1 1) (fromGregorian 2000 12 31)

p19Test = eulerTest "p19" 171 p19


