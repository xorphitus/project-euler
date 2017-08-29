import Data.List

limit :: Int
limit = 1000000

collatz :: Int -> Int
collatz n
  | odd n     = 3 * n + 1
  | otherwise = floor (fromIntegral n / 2)

collatzSeq :: Int -> [Int]
collatzSeq n = toSeq [n]
  where
    toSeq []  = []
    toSeq (x:xs)
      | x == 1    = reverse (x:xs)
      | otherwise = toSeq (collatz x : x : xs)

main :: IO ()
main = print $ maximumBy (\(x, _) (y, _) -> compare x y) $ map (\x -> (length $ collatzSeq x, x)) [1..limit]
