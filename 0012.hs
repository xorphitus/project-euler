-- WIP

triangle :: Int -> Int
triangle x =
  let x' = fromIntegral x
  in floor((1 + x') * x' / 2)

divisors :: Int -> [Int]
divisors x = filter (\y -> rem x y == 0) [1..x]

main :: IO ()
main = print
  $ head
  $ dropWhile (\(_, l) -> l > 500)
  $ map (\x -> (x, length $ divisors x))
  $ map triangle [1..]
