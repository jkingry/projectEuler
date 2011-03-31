import Data.List

rows x = x
columns x = transpose x 

{-
diaginols x = zipWith diaginol x [0..(length 
	where diaginol x d = zipWith (!!) [d..d + (length n) - 1] x

[_,_,0]
[_,0,1]
[0,1,2]
[1,2,_]
[2,_,_]

[0,1,2,3]
[1,2,3,_]
[2,3,_,_]
[_,0,1,2]
[_,_,0,1]
-}

diags :: [[a]] -> [[a]]
diags x = map (diag x) $ diagIndexes $ length x 

diagIndexes :: Int -> [[Maybe Int]]
diagIndexes n = leftDiags ++ rightDiags
        where leftDiags = allDiags [0..maxIndex]
              rightDiags = allDiags $ reverse [0..maxIndex]
              allDiags source = filter ((>1) . length) $ map (createDiag source) deltas 
              createDiag source d = map indexFilter (map (+d) source)
              indexFilter i = if i > maxIndex || i < 0 then Nothing else Just i
              deltas = [-maxIndex..maxIndex]
              maxIndex = (n - 1)

diag :: [[a]] -> [Maybe Int] -> [a]
diag x indexes = filterMaybe (zipWith elementAt x indexes)
    where elementAt list (Just i) = Just (list !! i)
          elementAt list Nothing = Nothing
          filterMaybe [] = []
          filterMaybe (Nothing:xs) = filterMaybe xs
          filterMaybe ((Just x):xs) = x:filterMaybe xs
