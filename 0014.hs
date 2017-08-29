-- WIP

limit :: Int
limit = 1000000

powsOf2 :: [Int]
powsOf2 = takeWhile (< limit) $ map (2 ^) [0..]

log2 :: Int -> Int
log2 = floor . logBase 2 . fromIntegral

collatz :: Int -> Int
collatz n
  | odd n     = 3 * n + 1
  | otherwise = floor (fromIntegral n / 2)

collatzSeq :: [Int] -> [Int]
collatzSeq []  = []
collatzSeq (x:xs)
  | x == 1    = reverse (x:xs)
  | otherwise = collatzSeq (collatz x : x : xs)

main :: IO ()
main = print $ collatzSeq [1]
