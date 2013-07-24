{-
 - Capítulo 1
 - Introducción a Haskell
 -}

{-
 - NOTAS:
 - No puedes usar funciones de Prelude como
 - map, foldr, foldl, etc
 -}
-- Ejercicio 1.12:
-- Define una función
aEntero :: [Integer] -> Integer
-- que transforme una lista de dígitos en el correspondiente valor entero:
-- aEntero [2,3,4] => 234
--
-- Define la función recíproca aLista:
aLista :: Integer -> [Integer]

-- Ejemplo 1.13
primero2 :: (Integer, Integer) -> Integer
primero (x,_) = x

primero3 :: (Integer, Integer, Integer) -> Integer
primero3(x,_,_) = x

-- Ejemplo 1.14
sumaPares :: [(Integer, Integer)] -> Integer
sumaPares [] = 0
sumaPares ((x,y):xs) = x + y + sumaPares xs

-- Ejemplo 1.16
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Ejemplo 1.17
longitud :: [Integer] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- Ejemplo 1.18
long :: [Integer] -> Integer
long ls = case ls of
            []   -> 0
            _:xs -> 1 + long xs

-- Ejemplo 1.19
cabeza :: [Integer] -> Integer
cabeza [] = error "cabeza de lista vacía no definida"
cabeza (x:_) = x

-- Ejemplo 1.20
absoluto :: Integer -> Integer
absoluto x
  | x >= 0 =  x
  | x < 0  = -x

-- Ejemplo 1.21
signo :: Integer -> Integer
signo x
  | x < 0     = -1
  | x == 0    =  0
  | otherwise =  1

-- Ejemplo 1.22
maxEnt :: Integer -> Integer -> Integer
maxEnt x y = if x > y then x else y

-- Ejemplo 1.23
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c
  | disc >= 0 = ((-b + raizDisc)/denom, (-b-raizDisc)/denom)
  | otherwise = error "raices complejas"
  where
    disc     = b ^ 2 - 4 * a * c
    raizDisc = sqrt disc
    denom = 2 * a

-- Ejercicio 1.24
-- Escribe una función
descomponer :: Integer -> (Integer, Integer, Integer)
-- que a partir de una cantidad de segundos, devuelva una terna
-- con las horas, minutos y segundos equivalentes.

{-
 -
 - Ejercicios
 -
 - -}

-- Ejercicio 1.26
-- Sea una lista de funciones de enteros en enteros
-- [f1,f2,f3,...,fn] :: [Integer -> Integer]
-- Define un operador
(|>) :: [Integer -> Integer] -> Integer -> [Integer]
-- [f1 x, f2 x, f3 x, ..., fn x]

-- Ejercicio 1.27
-- Escribe una función que determine si un año es bisiesto.
-- Un año es bisiesto si es múltiplo de 4 (por ejemplo, 1984).
-- Una excepción a la regla anterior es que los años múltiplos
-- de 100 sólo son bisiestos cuando a su vez son múltiplos de
-- 400 (por ejemplo, 1800 no es bisiesto, mientras que 2000 sí)
esBisiesto :: Integer -> Bool

-- Ejercicio 1.28
-- Escribe una función que calcule el número de días de un mes,
-- dados los valores numéricos de mes y año.
-- Nota: considera los años bisiestos para Febrero.
diasMes :: Integer -> Integer -> Integer

-- Ejercicio 1.29
-- Escribe una función que añada un dígito a la derecha de un
-- número entero:
aLaDerechaDe :: Integer -> Integer -> Integer

-- Ej: 3 `aLaDerechaDe` 146 => 1463

-- Ejercicio 1.30
-- Escribe una función recursiva que devuelva el resto de la
-- división de dos enteros usando sustracciones.
miMod :: Integer -> Integer -> Integer

-- Ejercicio 1.31
-- Escribe una función recursiva que devuelva el cociente que
-- se obtiene al dividir dos números enteros usando sumas y
-- restas
miDiv :: Integer -> Integer -> Integer

-- Ejercicio 1.32
-- Escribe una función recursiva que devuelva el sumatorio
-- desde un valor entero hasta otro.
sumDesdeHasta :: Integer -> Integer -> Integer

-- Ejercicio 1.33
-- Escribe una función recursiva que devuelva el producto
-- desde un valor entero hasta otro:
prodDesdeHasta :: Integer -> Integer -> Integer

-- Ejercicio 1.34
-- Escribe una función
variaciones :: Integer -> Integer -> Integer
-- que calcule el número de variaciones de m elementos
-- tomados de n en n. Usa para ello la siguiente relación
-- variaciones m n = m! / (m - n)!
-- Escribe otra versión que use ésta otra
variaciones2 :: Integer -> Integer -> Integer
-- variaciones m n = (m-n+1)*(m-n+2)*...*(m-1)*m


-- Ejercicio 1.35
-- Escribe una función que calcule números combinatorios
-- usando la siguiente relación:
combinatorio :: Integer -> Integer -> Integer

-- combinatorio m n = m! /(n!*(m-n)!)
-- Escribe otra versión que use estas relaciones:
combinatorio2 :: Integer -> Integer -> Integer
-- combinatorio m 0 = 1
-- combinatorio m m = 1
-- combinatorio m n = combinatorio (m-1) (n-1) +
--                    combinatorio (m-1) n
-- ¿Puedes garantizar que combinatorio2 acaba?

-- Ejercicio 1.36
-- Escribe una función que devuelva el i-ésimo número
-- de la sucesión de fibonacci. Esta sucesión tiene
-- como primer término 0, como segundo 1, y cualquier
-- otro término se obtiene sumando los dos que le preceden
-- 0,1,1,2,3,5,8,13,21,34,55...
fibonacci :: Integer -> Integer

-- Ejercicio 1.37
-- Escribe una función que determine el mayor de tres números
-- enteros. Escribe otro para cuatro números.
maximo3 :: Integer -> Integer -> Integer -> Integer

maximo4 :: Integer -> Integer -> Integer -> Integer -> Integer

-- Ejercicio 1.38
-- Escribe una función que tome tres números enteros y devuelva
-- una terna con los números ordenados en orden creciente
ordena3 :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)

-- Ejercicio 1.39
-- Escribe una función que determine si un número positivo
-- de exactamente cuatro cifras es capicúa o no:
esCapicua :: Integer -> Bool
-- esCapicua 1221 => True
-- esCapicua 12 => ERROR: número de cifras incorrecto

-- Ejercicio 1.40
-- Escribe una función que calcule la suma de las cifras
-- de un número natural
sumaCifras :: Integer -> Integer

-- sumaCifras 123 => 6

-- Ejercicio 1.41
-- Escribe una función que calcule el número de cifras
-- de un número natural (sin ceros a la izquierda)
numeroCifras :: Integer -> Integer
-- numeroCifras 123 => 3

-- Ejercicio 1.42
-- Escribe una función trocear que tome un número n
-- de dígitos y que, usando sólo sumas y restas, devuelva
-- un par donde el primer elemento corresponde alos n-1
-- primeros dígitos y el segundo elemento sea el dígito
-- n-ésimo.
trocear :: Integer -> (Integer, Integer)
-- trocear 1234 => (123,4)

-- Ejercicio 1.43
-- Escribe una función concatenar que concatene los digitos
-- de dos números no nulos.
concatenar :: Integer -> Integer -> Integer

-- concatenar 123 45 => 12345
-- concatenar 123 0  => 123
