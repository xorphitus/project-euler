import Data.List

eratosthenes :: Int -> [Int] -> [Int] -> [Int]
eratosthenes x [] zs = eratosthenes x [2..x] zs
eratosthenes x (y:ys) zs
  | y >= ceiling (sqrt $ fromIntegral x) = zs ++ (y:ys)
  | otherwise = eratosthenes x (filter (\n -> rem n y /= 0) ys) (zs ++ [y])

primes :: Int -> [Int]
primes x = eratosthenes x [] []

main :: IO ()
main = print
  $ fmap (!! 10000)
  $ find (\x -> length x >= 10001)
  $ map primes [100000,200000..] -- 小さい数から探索してたら遅すぎてやってらんない
