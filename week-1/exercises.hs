-- double function
double x = x + x

-- square function
square x = x * x

-- isEven function
isEven x = even x

-- sumOfSquares function
sumOfSquares x y = square x + square y

-- maxOfThree function
maxOfThree x y z = max x (max y z)

-- distance
distance x1 y1 x2 y2 = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- fib
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

-- factorial
factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- isPrime
isPrime n = null [x | x <- [2 .. n - 1], n `mod` x == 0]

-- main
main :: IO ()
main = do
  let result = distance 1 2 4 6
  print result