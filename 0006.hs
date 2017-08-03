squareSum :: [Int] -> Int
squareSum xs = sum $ map (^2) xs

sumSquare :: [Int] -> Int
sumSquare xs = sum xs ^ 2

main :: IO ()
main =
  let nums = [1..100]
  in print $ sumSquare nums - squareSum nums
