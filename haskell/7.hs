import EulerCommon (primes)

p7 n = last (take n primes)

main = print (p7 10001)
