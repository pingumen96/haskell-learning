-- Definizione della funzione double
double :: Int -> Int
double x = x + x

-- Esecuzione della funzione double con un valore di input
main :: IO ()
main = do
  let result = double (double 2)
  print result