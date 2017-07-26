-- WIP

{-|
 2 = 2^1
 3 =     3^1
 4 = 2^2
 5 =         5^1
 6 = 2^1 3^1
 7 =              7^1
 8 = 2^3
 9 =     3^2
10 = 2^1     5^1
11 =                  11^1
12 = 2^2 3^1
13 =                       13^1
14 = 2^1          7^1
15 =     3^1 5^1
16 = 2^4
17 =                            17^1
18 = 2^1 3^2
19 =                                 19^1
20 = 2^2     5^1

=> 2^4 * 3^2 * 5^1 * 7^1 * 13^1 * 17^1 * 19^1
-}

toMaxPrimeFactor :: Int -> Int
toMaxPrimeFactor = ceiling . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime x =
  let limit = toMaxPrimeFactor x
      list = [2] ++ [3,5..limit]
  in all (\n -> (rem x n) /= 0) list

primes :: Int -> [Int]
primes x = filter isPrime [2..x]

-- TODO
countPrime :: Int -> Int -> Int
countPrime n p = n

decomposeToPrimes :: Int -> [(Int, Int)]
decomposeToPrimes x =
  let p = primes x
  in map (\n -> (n, (countPrime x n))) p

-- TODO
aggregatePrimes :: [[(Int, Int)]] -> Int
aggregatePrimes xss =
  let flatten = foldr (++) []
      flattened = flatten xss
  in 10

main :: IO ()
main = print $ aggregatePrimes (map decomposeToPrimes [1..20])
