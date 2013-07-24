{-
 - Capítulo 2
 - Funciones de orden superior y polimorfismo
 -}

-- Ejemplo 2.0
multiploDe :: Integer -> Integer -> Bool
multiploDe p n = n `mod` p == 0

-- Se puede obtener entonces
esPar :: Integer -> Bool
esPar = multiploDe 2

-- Ejemplo 2.2
potencia :: Integer -> Integer -> Integer
potencia e b
  | b < 0 || e < 0  = error "parámetro(s) negativo(s)"
potencia 0 b = 1
potencia e b = b * potencia (e-1) b

alCuadrado :: Integer -> Integer
alCuadrado = potencia 2

-- dosVeces
dosVeces :: (Integer -> Integer) -> Integer -> Integer
dosVeces f x = f f x

-- Ejemplo 2.4
derivada :: (Float -> Float) -> Float -> Float
derivada f x = (f(x+h) - f x)/h
  where
    h = 0.0001

-- Ejemplo 2.5
logEnBase :: Float -> (Float -> Float)
logEnBase b = \x -> (log x) / (log b)

-- iter
iter :: (Integer -> Integer -> Integer) ->
        Integer ->
        Integer ->
        Integer
iter op e 0 = e
iter op e n = op n (iter op e (n-1))

-- Ejercicio 2.6
-- Escribe la función potencia como un caso particular
-- del combinador iter
potencia2 :: Integer -> Integer -> Integer

-- Ejercicio 2.7
-- ¿Cuál es el tipo más general de la función dosVeces?

-- Composición
-- infixr 9
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- g . f = \x. g(f x)

-- Ejemplo 2.8
-- Usando la composición podemos definir la función
-- esImpar a partir de la función esPar para comprobar
-- la partidad de un número.
esPar2, esImpar :: Integer -> Bool
esPar2 x = (x `mod` 2 == 0)
esImpar = not . esPar2

-- Ejemplo 2.9
-- El operador prefedinifo (++) permite concatenar dos listas
-- del mismo tipo disponiendo los elementos de la segunda
-- tras los de la primera.
-- [1,2,3] ++ [4,5] => [1,2,3,4,5]

-- Ejemplo 2.10
-- Función length
-- length :: [a] -> Int

-- Ejemplo 2.11
-- map :: (a -> b) -> [a] -> [b]

-- Ejemplo 2.12
-- Es posible defnir un operador que aplique una lista
-- de funciones a un dato, devolviendo una lista de
-- resultados:
(|>) :: [a -> b] -> a -> [b]
-- []     |> _ = []
-- (f:fs) |> x = f x : (fs |> x)
-- Puede ser definido como point free
(|>) = flip (map . flip ($))

-- Ejercicio 2.13
-- Analiza cuál es el tipo más general de las
-- siguientes funciones:
-- const x y = x
-- subst f g x = f x (g x)
-- flip f x y = f y x
-- curry f x y = f(x,y)
-- uncurry f (x,y) = f x y
-- pair (f,g) x = (f x, g x)
-- cross (f,g) (x,y) = (f x, g y)
-- Asimismo, comprueba los resultados con el
-- intérprete
