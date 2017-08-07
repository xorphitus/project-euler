import Data.List

{-|
Triangle theorem
a + b > c
∵ a^2 + b^2 = c^2
=> a^2 + b^2 + 2ab > c^2
=> (a + b)^2 > c^2
=> a + b > c ...(1)

a + b + c = 1000
=> c + c < 1000 (∵(1))
=> c < 500
-}
candidates :: [(Int, Int, Int)]
candidates = do
  c <- [3..500]
  a <- [1..(c - 2)]
  return (a, 1000 - c - a, c)

findAnswer :: [(Int, Int, Int)] -> Maybe (Int, Int, Int)
findAnswer = find (\(a, b, c) -> a < b && b < c) . filter (\(a, b, c) -> a ^ 2 + b ^ 2 == c ^ 2)

main :: IO ()
main = print $ (\(a, b, c) -> a * b * c) <$> findAnswer candidates
