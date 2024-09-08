luhnDouble :: Int -> Int
luhnDouble x = if x * 2 > 9 then (x * 2) - 9 else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + luhnDouble c + b + d) `mod` 10 == 0

main :: IO ()
main = do
  print (luhn 1 7 8 4)
  print (luhn 4 7 8 3)