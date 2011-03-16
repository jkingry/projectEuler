p1 n = sum (filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..n-1])

main = print (p1 1000)

