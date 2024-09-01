-- curried function
add :: Int -> (Int -> Int)
add x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

main :: IO ()
main = do
  let function = add 2
  let result = function 3
  print result