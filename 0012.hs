import Data.List

triangle :: Int -> Int
triangle x =
  let x' = fromIntegral x
  in floor((1 + x') * x' / 2)

divisors :: Int -> [Int]
divisors x =
  let divs = filter (\y -> rem x y == 0) [2..(floor (fromIntegral x / 2))]
  in [1, x] ++ divs

main :: IO ()
main = print
  $ find (\(_, l) -> l > 500)
  $ map (\x -> (x, length $ divisors x))
  $ map triangle [3..]
