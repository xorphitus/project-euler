import Data.List

triangle :: Int -> Int
triangle x =
  let x' = fromIntegral x
  in floor((1 + x') * x' / 2)

-- 総当たりでカウントしてると計算が終わらない
countDivisors :: Int -> Int
countDivisors x =
  let n = ceiling (sqrt $ fromIntegral x)
      divs = filter (\y -> rem x y == 0) [1..n]
      divlen = length divs
  in if (last divs) == n then divlen * 2 - 1 else divlen * 2

main :: IO ()
main = print
  $ find (\(_, l) -> l > 500)
  $ map (\x -> (x, countDivisors x))
  $ map triangle [3..]
