# Tipi

Un tipo è una collezione di valori in relazione tra loro.
Si usa la notazione v::T per indicare che v è di tipo T.

Esempio:
- False :: Bool
- True :: Bool
- not :: Bool -> Bool

In Haskell, ogni espressione deve avere un tipo, che viene calcolato prima di valutare l'espressione tramite un processo chiamato inferenza di tipo.
La chiave di questo processo è la seguente regola: se f è una funzione che mappa argomenti di tipo A a risultati di tipo B, ed e è un'espressione di tipo A, allora l'applicazione f e ha tipo B.

$$
\frac{f :: A \to B \quad e :: A}{f\ e :: B}
$$

not False :: Bool perché not :: Bool -> Bool e False :: Bool.
not 3 non è valido perché not :: Bool -> Bool e 3 :: Num a => a, quindi non è possibile applicare not a 3.

## Inferenza di tipo
Visto che l'inferenza di tipo avviene prima della valutazione dell'espressione, i programmi Haskell sono type safe, ovvero non è possibile avere errori di tipo. Nella pratica, l'inferenza rileva una vasta gamma di errori di programmazione.
L'inferenza di tipo non elimina la possibilità di errori a runtime, ad esempio divisione per zero.

1 'div' 0 -- Errore a runtime

## Type safety
Lo svantaggio della type safety è che alcune espressioni che verrebbero valutate correttamente verranno rifiutate sulla base del tipo.

if True then 1 else False -- Errore di tipo

In GHCi è possibile mostrare il tipo di ogni espressione precedendola con :type, ad esempio:
:type False
False :: Bool

## Tipi di base
I tipi di Haskell sono:
- Bool: valori True e False
- Char: caratteri Unicode
- String: sequenze di caratteri
- Int: numeri interi limitati
- Integer: numeri interi illimitati
- Float: numeri in virgola mobile a precisione singola
- Double: numeri in virgola mobile a precisione doppia

## Tipi lista
[False, True] :: [Bool]
['a', 'b', 'c'] :: [Char]
[1, 2, 3] :: [Int]
[1.0, 2.0, 3.0] :: [Float]

## Tipi tupla
(False, True) :: (Bool, Bool)
('a', 'b', 'c') :: (Char, Char, Char)
(1, 2, 3) :: (Int, Int, Int)
(1.0, 2.0, 3.0) :: (Float, Float, Float)

Il numero di componenti di una tupla è chiamato arità. La tupla () di arità 0 è chiamata tupla vuota. Le tuple di arità 2 sono chiamate coppie, ecc.
Le tuple di arità 1, come (False), non sono permesse perché l'uso delle parentesi entrerebbe in confitto con la notazione delle espressioni.

('a', (False, 'b')) :: (Char, (Bool, Char))
(['a', 'b'], [False, True]) :: ([Char], [Bool])
[('a', False), ('b', True)] :: [(Char, Bool)]

Le tuple devono avere una arità finita, in modo che si possa fare l'inferenza di tipo prima della valutazione.

## Tipi funzione
Una funzione è una mappatura di argomenti di un tipo verso risultati di un altro tipo.
Scriviamo T1 -> T2 per indicare il tipo di una funzione che mappa valori di tipo T1 a valori di tipo T2.

not :: Bool -> Bool
even :: Int -> Bool

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

## Funzioni curried
Le funzioni con più argomenti possono essere gestite in un altro modo, sfruttando il fatto che le funzioni sono libere di restituire funzioni come risultati.

```haskell
add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

main :: IO ()
main = do
  let function = add 2
  let result = function 3
  print result
```

Per evitare troppe parentesi, la funzione -> è associativa a destra, dunque:
Int -> Int -> Int -> Int
equivale a:
Int -> (Int -> (Int -> Int))

Di conseguenza, l'applicazione della funzione
mult x y z
può essere scritta come
((mult x) y) z

A meno che non sia necessario il tupling, tutte le funzioni multivariate in Haskell sono normalmente curried, e le due convenzioni di cui sopra sono utili a ridurre il numero di parentesi.

## Tipi polimorfici
La funzione di libreria length calcola la lunghezza di una lista, indipendentemente dal tipo degli elementi della lista.

> length [1, 2, 3]
3

> length ['a', 'b', 'c']
3

Questo è possibile grazie al concetto di variabile di tipo. Le variabili di tipoo devono iniziare con una lettera minuscola e sono normalmente nominate come a, b, c, ecc.

length :: [a] -> Int

Un tipo che contiene una o più variabili di tipo è detto polimorfico.
Molte delle funzioni di libreria di Haskell sono polimorfiche:
- fst :: (a, b) -> a
- head :: [a] -> a
- take :: Int -> [a] -> [a]
- zip :: [a] -> [b] -> [(a, b)]
- id :: a -> a

Il tipo di una funzione polimorfica spesso dà una forte indicazione di ciò che fa la funzione.

## Tipi overloaded
L'operatore + calcola la somma di due numeri dello stesso tipo. L'idea che + possa essere applicato a numeri di ogni tipo numerico è resa precisa nel suo tipo attraverso l'inclusione di un vincolo di classe.
I vincoli di classe sono scritti nella forma C a, dove C è il nome della classe e a è una variabile di tipo.

(+) :: Num a => a -> a -> a

Ovvero, per ogni tipo a istanza della classe Num dei tipi numerici, la funzione (+) ha tipo a -> a -> a.
Un tipo che contiene uno o più vincoli di classe è detto overloaded (sovraccaricato), così come lo è un'espressione con tale tipo.
Dunque Num a => a -> a -> a è un tipo overloaded e (+) è una funzione overloaded.

(*) :: Num a => a -> a -> a
(/) :: Fractional a => a -> a -> a
(^) :: (Num a, Integral b) => a -> b -> a
negate :: Num a => a -> a
abs :: Num a => a -> a

Anche i numeri stessi sono overloaded:
3 :: Num a => a

## Classi base
Un tipo è una collezione di valori in relazione tra loro. Costruendo su questa idea, una classe è una collezione di tipi che supportano certe operazioni overloaded chiamate metodi.
Haskell fornisce una serie di classi base, tra cui:

### Eq
Tipi che supportano l'uguaglianza.

(==) :: Eq a => a -> a -> Bool
(/=) :: Eq a => a -> a -> Bool

> False == False
True

> 'a' == 'b'
False

> "abc" == "abc"
True

> [1, 2, 3] == [1, 2]
False

> ('a', False) == ('a', False)
True

### Ord
Tipi che sono istanze della classe Eq i quali valori sono totalmente (linearmente) ordinati e dunque possono essere confrontati usando i seguenti metodi:

(<) :: Ord a => a -> a -> Bool
(<=) :: Ord a => a -> a -> Bool
(>) :: Ord a => a -> a -> Bool
(>=) :: Ord a => a -> a -> Bool
min :: Ord a => a -> a -> a
max :: Ord a => a -> a -> a

Tutti i tipi base Bool, Char, String, Int, Integer, Float e Double sono istanze della classe Ord, così come le liste e le tuple (se i loro elementi lo sono).

> False < True
True

> min 'a' 'b'
'a'

> "elegant" < "elephant"
True

Le stringhe, le liste e le tuple sono ordinate lessicograficamente.

### Show
Tipi che possono essere convertiti in stringhe.

show :: Show a => a -> String

Tutti i tipi di base sono instanze della classe Show, così come le liste e le tuple (se i loro elementi lo sono).

> show False
"False"

> show 'a'
"'a'"

> show [1, 2, 3]
"[1, 2, 3]"

> show ('a', False)
"('a', False)"

### Read
Tipi che possono essere costruiti da stringhe, rappresenta la classe duale di Show.

read :: Read a => String -> a

Tutti i tipi di base sono istanze della classe Read, così come le liste e le tuple (se i loro elementi lo sono).

> read "False" :: Bool
False

> read "'a'" :: Char
'a'

> read "[1, 2, 3]" :: [Int]
[1, 2, 3]

### Num
Tipi che sono numeri.

(+) :: Num a => a -> a -> a
(-) :: Num a => a -> a -> a
(*) :: Num a => a -> a -> a
negate :: Num a => a -> a
abs :: Num a => a -> a
signum :: Num a => a -> a

Num non fornisce un metodo per la divisione, perché non tutti i tipi numerici supportano la divisione.

### Integral
Tipi che sono istanze della classe Num e supportano le operazioni di divisione e modulo.

div :: Integral a => a -> a -> a
mod :: Integral a => a -> a -> a

> 7 `div` 2
3

> 7 `mod` 2
1

### Fractional
Tipi che sono istanze della classe Num ma i quali valori sono non-interi e in quanto tali supportano la divisione e il calcolo del reciproco.

(/) :: Fractional a => a -> a -> a
recip :: Fractional a => a -> a

I tipi Float e Double sono istanze della classe Fractional.

> 7.0 / 2.0
3.5

> recip 2.0
0.5

# Esercizi
1. Quali sono i tipi dei seguenti valori?
- ['a', 'b', 'c'] => [Char]
- ('a', 'b', 'c') => (Char, Char, Char)
- [(False, '0'), (True, '1')] => [(Bool, Char)]
- ([False, True], ['0', '1']) => ([Bool], [Char])
- [tail, init, reverse] => [[a] -> [a]]

2. Scrivi le definizioni che hanno i seguenti tipi; non importa cosa fanno se sono type correct.
- bools :: [Bool]
bools = [True, False]

- nums :: [[Int]]
nums = [[1 :: Int, 2, 3], [4, 5, 6]]

- add :: Int -> Int -> Int -> Int
add x y z = x + y + z

- copy :: a -> (a, a)
copy x = (x, x)

- apply :: (a -> b) -> a -> b
apply f x = f x

3. Quali sono i tipi delle seguenti funzioni?
- second xs = head (tail xs)
[a] -> a

- swap (x, y) = (y, x)
(a, b) -> (b, a)

- pair x y = (x, y)
a -> b -> (a, b)

- double x = x * 2
Num a => a -> a

- palindrome xs = reverse xs == xs
Eq a => [a] -> Bool

- twice f x = f (f x)
(a -> a) -> a -> a
Riscrivibile come `twice f = f . f`

4. ...

5. Perché non è possibile in generale per i tipi di funzione essere istanze della classe Eq? Quando è possibile? Suggerimento: due funzioni dello stesso tipo sono uguali se restituiscono sempre risultati uguali per argomenti uguali.
Due funzioni sono uguali quando
