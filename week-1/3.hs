-- qsort
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- main
main :: IO ()
main = do
  let result = qsort [10, 2, 5, 3, 1, 6, 7, 4, 2, 3]
  print result