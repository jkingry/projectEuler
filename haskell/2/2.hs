fib n = fibs!!n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

p2 limit = sum (filter even (takeWhile (\n -> n < limit) fibs))

main = print (p2 4000000)
