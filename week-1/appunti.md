In Haskell, una funzione è una mappatura che prende uno o più argomenti e restituisce un risultato. È definita usando un'equazione che dà il nome alla funzione, un nome per ogni argomenti e un corpo che specifica come il risultato è calcolato in termini degli argomenti.

Ad esempio, la funzione `double` che mappa un numero intero `x` al suo doppio è definita come segue:

```haskell
double x = x + x
```

Operazioni su liste in Haskell:
- `head` restituisce il primo elemento di una lista
- `tail` restituisce la lista senza il primo elemento
- `length` restituisce la lunghezza di una lista
- `null` restituisce `True` se la lista è vuota, `False` altrimenti
- `reverse` inverte una lista
- `take n xs` restituisce i primi `n` elementi di una lista `xs`
- `drop n xs` restituisce la lista `xs` senza i primi `n` elementi
- `maximum` restituisce il massimo elemento di una lista
- `minimum` restituisce il minimo elemento di una lista
- `sum` restituisce la somma degli elementi di una lista
- `product` restituisce il prodotto degli elementi di una lista
- `elem x xs` restituisce `True` se l'elemento `x` è presente nella lista `xs`, `False` altrimenti
- `xs ++ ys` concatena due liste `xs` e `ys`