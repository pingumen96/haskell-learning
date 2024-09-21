# List comprehension
## Concetti di base
In matematica abbiamo quanto segue:
{ x^2 | x ∈ {1 .. 5} } = {1, 4, 9, 16, 25}
In Haskell questo diventa:

```haskell
[x^2 | x <- [1..5]] -- x^2 tale che x appartiene a [1..5]
```

Il simbolo `|` si legge "tale che", e `<-` si legge "appartiene a". Quindi la lista `[1..5]` viene mappata con la funzione `x^2` per ogni elemento `x` appartenente alla lista.

```haskell
[(x, y) | x <- [1, 2, 3], y <- [4, 5]] -- prodotto cartesiano delle due liste
```
Se si inverte l'ordine delle due liste, si ottiene un risultato diverso:

```haskell
[(x, y) | y <- [4, 5], x <- [1, 2, 3]] -- prodotto cartesiano delle due liste
```

I generatori successivi al primo possono dipendere dalle variabili generate in precedenza:

```haskell
[(x, y) | x <- [1..3], y <- [x..3]] -- prodotto cartesiano delle due liste
```

Per esempio, la funzione concat che concatena una lista di liste può essere definita usando un generatore per selezionare una lista e un generatore per selezionare ogni elemento di quella lista:

```haskell
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs] -- x tale che x appartiene a xs, dove xs appartiene a xss
```

Il wildcard `_ può essere usato per scartare certi elementi da una lista:
```haskell
firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length :: [a] -> Int
length xs = sum [1 | _ <- xs]
```

## Guardie
Nella list comprehension si possono usare delle guardie per filtrare gli elementi generati:
```haskell
[x | x <- [1..10], even x] -- x tale che x appartiene a [1..10] e x è pari

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]
```
Per decidere che un numero come 15 non è primo, non è necessario calcolare tutti i suoi fattori, perché con la lazy evaluation il calcolo si ferma non appena viene trovato un fattore diverso da 1 o da n.

```haskell
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
```

Supponiamo di rappresentare una tabella di lookup con una lista di coppie key-value. Una funzione find che restituisce la lista di tutti i valori associati con una data chiave può essere definita come segue:
```haskell
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

find 'b' [('a', 1), ('b', 2), ('c', 3), ('b', 4)] -- [2, 4]
```

## Zip
La funzione di libreria zip produce una nuova lista accoppiando gli elementi successivi di due liste esistenti finché una delle due liste finisce:
```haskell
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)
-- [1, 2, 3, 4] [2, 3, 4] -> [(1, 2), (2, 3), (3, 4)]
pairs [1, 2, 3, 4] -- [(1, 2), (2, 3), (3, 4)]

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']
```

## String comprehensions