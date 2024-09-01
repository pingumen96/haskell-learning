-- sequencing
seqn [] = return []
seqn (act : acts) = do
  x <- act
  xs <- seqn acts
  return (x : xs)

main :: IO ()
main = do
  let actions = [getChar, getChar, getChar]
  xs <- seqn actions
  print xs