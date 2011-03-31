import EulerCommon

p2 limit = sum (filter even (takeWhile (<limit) fibs))

main = print (p2 4000000)
