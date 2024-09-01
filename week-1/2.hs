-- main
main :: IO ()
main = do
  -- sum has the type Num a => [a] -> a
  let result = sum [1 .. 10]
  print result