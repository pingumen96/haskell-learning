safetail :: [a] -> [a]
safetail [] = []
safetail (_ : xs) = xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

main :: IO ()
main = do
  print (safetail [1, 2, 3])
  print (safetail [] :: [Int])
  print (safetail' [1, 2, 3])
  print (safetail' [] :: [Int])