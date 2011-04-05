module P25 (p25,p25Test) where

import EulerCommon

fp25 n xs = fst $ head $ dropWhile ((<n).length.show.snd) $ zip [0..] xs

p25 = return $ fp25 1000 fibs

p25Test = eulerTest "p25" 4782 p25