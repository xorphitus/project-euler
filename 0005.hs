import Data.List

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

=> 2^4 * 3^2 * 5^1 * 7^1 * 11^1 * 13^1 * 17^1 * 19^1
-}

isPrime :: Int -> Bool
isPrime x
  | x == 2    = True
  | otherwise =
    let limit = ceiling $ sqrt $ fromIntegral x
        list = [2] ++ [3,5..limit]
    in all (\n -> (rem x n) /= 0) list

primes :: Int -> [Int]
primes x = filter isPrime [2..x]

-- >>> countPrime 3 3
-- 1
-- >>> countPrime 9 3
-- 2
-- >>> countPrime 18 3
-- 2
countPrime :: Int -> Int -> Int
countPrime n p =
  let factors = map fst
        $ filter ((== 0) . (rem n) . snd)
        $ takeWhile (\x -> snd x <= n)
        $ map (\x -> (x, p ^ x)) [1..]
  in case factors of
    [] -> 0
    xs -> last xs

-- >>> decomposeToPrimes 3
-- [(3, 1)]
-- >>> decomposeToPrimes 12
-- [(2, 2), (3, 1)]
-- >>> decomposeToPrimes 90
-- [(2, 1), (3, 2), (5, 1)]
decomposeToPrimes :: Int -> [(Int, Int)]
decomposeToPrimes x =
  let p = primes x
  in filter (\y -> (snd y) > 0) $ map (\n -> (n, (countPrime x n))) p

-- >>> aggregatePrimes [[(2, 1), (3, 2)], [(2, 3), (5, 1)], [(2, 1), (3, 1)]]
-- [(2, 3), (3, 2), (5, 1)]
aggregatePrimes :: [[(Int, Int)]] -> [(Int, Int)]
aggregatePrimes xss =
  let flatten = foldr (++) []
      order = \x y ->
        let o = compare (fst x) (fst y)
        in case o of
          EQ -> compare (snd x) (snd y)
          e  -> e
  in map last
    $ groupBy (\x y -> fst x == fst y)
    $ sortBy order
    $ flatten xss

-- >>> multiply [(2, 1), (3, 2)]
-- 18
multiply :: [(Int, Int)] -> Int
multiply xs = foldr (*) 1 $ map (\x -> (fst x) ^ (snd x)) xs

main :: IO ()
main = print $ multiply $ aggregatePrimes (map decomposeToPrimes [1..20])
