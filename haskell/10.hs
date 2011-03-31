import EulerCommon (primes)

p10 = sum $ takeWhile (<2000000) primes

main = print p10
