{-
 - Capítulo 3
 - Definiciones de tipo
 -}

{-
 - 3.1 Parcialización
 -}

inc :: Integer -> Integer
-- inc x = x + 1
inc = (+ 1)

{-
 - Regla de lambda abstracciones
 - \x y z -> e => \x -> (\y -> (\z -> e))
 -}

sumaCuadrados :: Integer -> Integer -> Integer
-- sumaCuadrados :: Integer -> (Integer -> Integer) -- equivalente a la anterior
sumaCuadrados = \x -> (\y -> x*x + y*y)

{-
 - Ejemplo 3.1
 -}
multiploDe :: Integer -> Integer -> Bool
multiploDe p n = n `mod` p == 0

esPar :: Integer -> Bool
esPar = multiploDe 2

{-
 - Ejemplo 3.2
 -}

fun :: Int -> Int -> Int -> Int
fun x y z = x * (2 * y + z)

fun2 :: Int -> Int -> Int
fun2 = fun 5

fun3 :: Int -> Int
fun3 = fun 5 6

constante :: Int
constante = fun 5 6 7

{-
 - Ejemplo 3.3
 -}

multiploDee :: (Integer, Integer) -> Bool
multiploDee (p,n) = n `mod` p == 0
-- No admite parcialización

{-
 - Ejemplo 3.4
 -}
g = (+)
f = g 5
-- f = (5+)

{-
 - Ejemplo 3.5
 -}

derivada :: (Float -> Float) -> Float -> Float
derivada f x = (f (x + h) - f x) / h
  where
    h = 0.0001

{-
 - Ejemplo 3.6
 -}
logEnBase :: Float -> Float -> Float
logEnBase b = \x -> (log x) / (log b)

{-
 - Iter
 -}
iter :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
iter op e 0 = e
iter op e n = op n (iter op e (n-1))

factorial :: Integer -> Integer
factorial = iter (*) 1

sumatorio :: Integer -> Integer
sumatorio = iter (+) 0

{-
 - Ejercicio 3.7
 - Escriba la siguiente función potencia a través del combinador iter
 -}

potencia :: Integer -> Integer -> Integer
potencia b 0 = 1
potencia b n = b * potencia b (n - 1)

{-
 - Ejercicio 3.8
 - ¿Cuál es el tipo más general de la función dosVeces?
 -}
dosVeces f x = f (f x)

{-
 - Ejemplo 3.9
 -}
esImpar = not . esPar

{-
 - Ejemplo 3.10
 -}
-- [1,2,3] ++ [5,6] => [1,2,3,5,6]

{-
 - Ejemplo 3.11
 -}

lengthChar :: [Char] -> Int
lengthChar [] = 0
lengthChar (_:xs) = 1 + lengthChar xs

lengthP :: [a] -> Int
lengthP [] = 0
lengthP (_:xs) = 1 + length xs

{-
 - Ejemplo 3.12
 -}
-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = (f x) : map f xs

{-
 - Ejemplo 3.13
 -}
(|>) :: [a -> b] -> a -> [b]
(f : fs) |> x = (f x) : (fs |> x)

{-
 - Ejercicio 3.14
 - Analice cuál es el tipo más general de
 - las siguientes funciones, comprobar con
 - el intérprete:
 -}
-- const x y = x
-- subst f g x = f x (g x)
-- flip f x y = f y x
-- curry f x y = f (x, y)
-- uncurry f (x,y) = f x y
-- pair (f,g) x = (f x, g x)
-- cross (f,g) (x,y) = (f x, g y)

{-
 - Iterador polimórfico
 -}

iterGral :: (Integer -> a -> a) -> a -> Integer -> a
iterGral op e 0 = e
iterGral op e n = op n (iterGral op e (n - 1))

listaDecre :: Integer -> [Integer]
listaDecre = iterGral (:) []

-- listaDecre 5
-- [5,4,3,2,1] :: [Integer]

palos :: Integer -> String
palos = iterGral (\n xs -> '|' : xs) []

-- palos 3
-- "|||" :: String
