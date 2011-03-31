import EulerCommon
import Math.Sieve.Factor
-- triNumbers = 0 : zipWith (+) [1..] triNumbers 

triNumbers = map triNumber [1..] 
triNumber n = sum [1..n]

p12 n list = head $ dropWhile ((<n) . factorCount) list 
    where s = sieve $ 2^30
          factorCount n = product $ map ((+1) . snd) (factor s n) 

main = print (p12 500 triNumbers)



