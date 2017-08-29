toInt :: Char -> Int
toInt x = read [x] :: Int

main :: IO ()
main = print $ sum $ map toInt $ show $ 2^1000
