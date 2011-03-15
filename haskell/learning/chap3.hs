-- exercise 1,2
listLen :: [a] -> Integer
listLen [] = 0
listLen (x:xs) = 1 + listLen xs

-- exercise 3
mean list = (sum list) / fromIntegral (length list)

-- exercise 4
palin [] = []
palin [a] = [a]
palin (a:b) = a:((palin b) ++ [a])

-- exercise 5
isPalindrome list = list == reverse list

-- exercise 6 ??
-- sortByLength list = sortBy length list

-- exercise 7
inter:: a -> [[a]] -> [a]
inter sep [] = []
inter sep [x] = x
inter sep (x:xs) = x ++ (sep:(inter sep xs))

-- exercise 8
data Tree a = Node a (Tree a) (Tree a)
	| Empty

height Empty = 0
height (Node a left right) = 1 + max (height left) (height right)

-- exercise 9
data Direction = Lft | Rgt | Str deriving (Show)

-- exercise 10
-- turn (ax,ay) (bx,by) (cx,cy) 
--	| my == cy = Str
--	| my < cy = Lft 
--	| my > cy = Rgt 
--	| otherwise = error (show my)
--	where m = (by - ay) / (bx - ax)
--              b = ay - (m*ax)
--	      my = case m of
--		NaN -> axm * cx + b
	



