{-
 - Capítulo 4
 - Definición de tipos
 -}

{-
 - Types
 -}

type Entero = Integer

suma :: Entero -> Entero -> Entero
suma = (+)
-- Compila!
-- Entero es realmente lo mismo que Integer
-- El tipo más conocido, sin duda, es String = [Char]

{-
 - Data
 -}

data DiaSemana = Lunes | Martes | Miercoles | Jueves
  | Viernes | Sabado | Domingo deriving Show

laborables :: [DiaSemana]
laborables = [Lunes, Martes, Miercoles, Jueves, Viernes]

esFinSemana :: DiaSemana -> Bool
esFinSemana Sabado = True
esFinSemana Domingo = True
esFinSemana _ = False

{-
 - Bool está declarado en prelude del siguiente modo
 - data Bool = True | False
 -            deriving(Eq, Ord, Ix, Enum, Read, Show, Bounded)
 -}

{-
 - Ejercicio 4.1
 - Escriba una función que a partir de tres números d, m y a, que
 - representan una fecha (día, mes, año), calcule el día, (con tipo
 - DiaSemana) de la semana correspondiente usando la congruencia de
 - Zeller:
 - dia = (700 + (26x-2) `div` 10 + d + y
 -        + y `div` 4 + z `div` 4 - 2z) `mod` 7
 -
 - donde los valores de x, y, z son:
 -    * Si m <= 2
 -      * x = m + 10
 -      * y = (a - 1) `mod` 100
 -      * z = (a - 1) `div` 100
 -    * Si m > 2
 -      * x = m - 2
 -      * y = a `mod` 100
 -      * z = a `div` 100
 - El valor de día estará entre 0 y 6 (0 significa domingo, 1 lunes, etc.)
 -
 - Nota: este método sólo es válido para el calendario gregoriano, que fue
 - introducido en distintos países en distintas fechas (el 14 de septiembre
 - de 1942 en Inglaterra, por ejemplo)
 -}

{-
 - Uniones de datos
 -}

data LetraOEntero = Letra Char | Entero Integer

{-
 - Productos
 -}

data Racional = Par Integer Integer deriving Show

-- Par :: Integer -> Integer -> Racional
unMedio :: Racional
unMedio = Par 1 2

numerador :: Racional -> Integer
numerador (Par x _) = x

denominador :: Racional -> Integer
denominador (Par _ x) = x

por :: Racional -> Racional -> Racional
(Par a b) `por` (Par c d) = Par (a * c) (b * d)

-- Persona
type Nombre = String
type Apellido = String
type Edad = Integer
data Persona = UnaPersona {
                            nombre    :: Nombre,
                            apellido1 :: Apellido,
                            apellido2 :: Apellido,
                            edad      :: Edad
                          } deriving Show
juan :: Persona
juan = UnaPersona {
                    nombre = "Juan",
                    apellido1 = "Rodríguez",
                    apellido2 = "García",
                    edad = 33
                  }

-- :t edad
-- edad :: Persona -> Edad
-- :t apellido1
-- apellido1 :: Persona -> Apellido

{-
 - Ejemplo 4.2
 -}
cumpleaños :: Persona -> Persona
cumpleaños p = p { edad = 1 + edad p }

{-
 - Si el constructor del dato es binario, puede ser simbólico
 - su nombre, pero siempre empezando por dos puntos
 -}

data Complejo = Float :- Float deriving Show
origen :: Complejo
origen = 0.0 :- 0.0

parteReal :: Complejo -> Float
parteReal (x :- _) = x

{-
 - Ejemplo 4.3
 -}

type Radio = Float
type Lado = Float
data Figure = Circulo Radio
            | Cuadrado Lado
            | Rectangulo Lado Lado
            | Punto
            deriving Show

{-
 - Ejercicio 4.4
 - Definir
 - area :: Figura -> Area
 - perimetro :: Figura -> Perimetro
 -}
type Area = Float
type Perimetro = Float

{-
 - Ejercicio 4.5
 - Dadas las siguientes declaraciones de tipo:
 -}
data Resultado = UnaReal Float
                | DosReales Float Float
                | DosComplejas Complejo Complejo
                deriving Show

{-
 - completar la función que devuelve las raíces de la
 - ecuación ax^2 + bx + c = 0
 - raices :: Float -> Float -> Float -> Resultado
 -}

{-
 - Tipos recursivos: Los naturales
 -}
data Nat = Cero | Suc Nat deriving Show

uno :: Nat
uno = Suc Cero
dos :: Nat
dos = Suc uno

-- Indefinido
indefinidoN :: Nat
indefinidoN = undefined

{-
 - indefinidoN se reduce a bottom
 - Suc indefinidoN :: Nat, se reduce a
 - Suc bottom
 - Por ejemplo, para la siguiente función:
 -}

esCero :: Nat -> Bool
esCero Cero = True
esCero _ = False
{-
 - Devuelve False para Suc indefinidoN pero
 - no devuelve nada para indefinidoN
 - Se puede decir que Suc indefinidoN no está
 - definido, pero está más definido que indefinidoN
 -}

infinitoN :: Nat
infinitoN = Suc infinitoN

esPar :: Nat -> Bool
esPar Cero = True
esPar (Suc x) = not . esPar $ x

{-
 - suma
 -}
infixl 6 <+>
(<+>) :: Nat -> Nat -> Nat
m <+> Cero = m
m <+> (Suc n) = Suc (m <+> n)
{-
 - Pregunta bonus:
 - ¿Por qué esta definición y no:
 - m <+> (Suc n) = (Suc m) <+> n
 - ?
 -}

{-
 - producto
 -}
infixl 7 <*>
(<*>) :: Nat -> Nat -> Nat
m <*> Cero = Cero
m <*> (Suc n) = m <*> n <+> m

{-
 - potencia
 -}
infixr 8 <^>
(<^>) :: Nat -> Nat -> Nat
b <^> Cero = uno
b <^> (Suc e) = b <*> b <^> e

{-
 - Ejercicio 4.6
 - Defina las funciones
 -}

-- divNat :: Nat -> Nat -> Nat
-- modNat :: Nat -> Nat -> Nat

{-
 - que calculen respectivamente el cociente entero
 - y el resto de dividir dos número naturales
 -}

{-
 - Ejercicio 4.7
 - Defina los operadores <*> y <^> de modo que analicen
 - el primer argumento en vez del segundo. la definición
 - de potencia se complica bastante, pero es posible si
 - se utiliza el binomio de Newton
 - (x + 1) ^ n =
 - 1 + sum_{i = 1}^{n}{n \choose i} * x ^ i
 -}

{-
 - Ejemplo 4.8
 - Otro ejemplo de tipo recursivo es el siguiente, que puede
 - usarse para representar expresiones aritméticas simples
 - sobre enteros
 -}

data Expr = Valor Integer
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          deriving Show

{-
 - En esta declaración (:+:), (:-:) y (:*:) son constructores
 - infijos de datos.
 -}

numOpers :: Expr -> Integer
numOpers (Valor _) = 0
numOpers (e1 :+: e2) = (numOpers e1) + (numOpers e2) + 1
numOpers (e1 :-: e2) = (numOpers e1) + (numOpers e2) + 1
numOpers (e1 :*: e2) = (numOpers e1) + (numOpers e2) + 1

{-
 - Ejercicio 4.9
 - Defina una función que calcule el valor de una expresión.
 - Por ejemplo:
 -}

-- valorDe :: Expr -> Integer
-- valorDe ((Valor 5) :+: (Valor 3)) => 8

{-
 - Ejercicio 4.10
 - Defina una función que calcule cuántas constantes enteras
 - aparecen en una expresión. Por ejemplo:
 -}
-- numConsts :: Expr -> Integer
-- numConsts ((Valor 5) :+: (Valor 3)) => 2

{-
 - Ejercicio 4.11
 - Defina una función que calcule el nivel de anidamiento máximo
 - de un operador en una expresión. Por ejemplo:
 -}

-- anidMax :: Expr -> Integer
-- annidMax (Valor 1 :+: (Valor 2 :*: (Valor 4 :*: Valor 3))) => 3

{-
 - Funciones de plegado
 -}

foldNat :: (a -> a) -> a -> (Nat -> a)
foldNat f e = fun
  where
    fun Cero = e
    fun (Suc n) = f . fun $ n

esParP :: Nat -> Bool
esParP = foldNat not True

{-
 - Ejercicio 4.12
 - Defina el producto de naturales (<*>) a través de foldNat
 -}

{-
 - Las funciones de plegado para tipos recursivos requieren de
 - un argumento por constructor
 -}

foldExpr :: (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) ->
            (Integer -> a) -> (Expr -> a)

foldExpr f g h j = fun
  where
    fun (Valor n) = j n
    fun (e1 :+: e2) = (fun e1) `f` (fun e2)
    fun (e1 :-: e2) = (fun e1) `g` (fun e2)
    fun (e1 :*: e2) = (fun e1) `h` (fun e2)

valorDeP :: Expr -> a
valorDeP = foldExpr (+) (-) (*) id

{-
 - Ejercicio 4.13
 - Defina una función que calcule cuántas constantes enteras aparecen
 - en una expresión usando foldExpr
 -}

{-
 - Ejercicio 4.14
 - Defina una función que calcule el nivel de anidamiento máximo de
 - un operador en una expresión usando foldExpr
 -}

{-
 - Tipos parametrizados
 -}

data Par a = UnPar a a deriving Show

{-
 - Un tipo de dato parametrizado es
 - data Either a b = Left a | Right b
 -                  deriving  (Eq, Ord, Read, Show)
 -}

{-
 - Un tipo de dato parametrizado es
 - data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)
 -}

{-
 - Ejemplo 4.15
 - Sean las siguientes declaraciones para representar una agenda de
 - teléfonos mediante una lista de pares
 -}

type Telefono = Integer
data Agenda = Ag [(Nombre, Telefono)]

-- Por ejemplo
unaAgenda :: Agenda
unaAgenda = Ag [("Pepe", 952274465),
                ("Luis", 956336518),
                ("Pedro", 952448615)]

{-
 - Escriba una función que permita buscar el teléfono de una persona
 - en una agenda
 -}
-- buscar :: Agenda -> Nombre -> Telefono

{-
 - newtype
 -}
newtype Natural = UnNatural Integer deriving Show

{-
 - Podemos restringir a sólo positivos con una función de conversión
 -}
aNatural :: Integer -> Natural
aNatural x
  | x < 0 = error "No se puede usar enteros negativos"
  | otherwise = UnNatural x

desdeNatural :: Natural -> Integer
desdeNatural (UnNatural x) = x

{-
 - Propiedades de funciones
 -}

{-
 - (f1 . f2) . f3 = f1 . (f2 . f3)
 -}

{-
 - Ejemplo 4.16
 -}

codigo :: Nat -> Integer
codigo Cero = 0
codigo (Suc n) = 1 + codigo n

{-
 - Ejemplo 4.17
 -}
espejo :: Expr -> Expr
espejo (Valor n) = Valor n
espejo (e1 :+: e2) = (espejo e2) :+: (espejo e1)
espejo (e1 :-: e2) = (espejo e2) :-: (espejo e1)
espejo (e1 :*: e2) = (espejo e2) :*: (espejo e1)

{-
 - Ejercicio 4.18
 - Demostrar por inducción estructural que id = espejo . espejo
 -}

{-
 - Ejercicio 4.19
 - Demostrar que [para todo natural, g(Cero) = e y
 - g(Suc n) = f(g n)] si y sólo si [g = foldNat f e]
 -
 - El sentido inverso es obvio, por definición de foldNat.
 - Para demostrar el sentido directo, hay que demostrar por
 - inducción sobre n que g n = foldNat f e n, para todo n
 - natural.
 -}

{-
 - Ejemplo 4.20
 - De la propiedad del ejercicio 4.19 podemos deducir:
 -}
-- esPar = foldNat not True

{-
 - Ejemplo 4.21
 - De la propiedad del ejercicio 4.19 podemos deducir:
 -}
-- Nota: y es (<+> n)
-- (<+>) = foldNat (\ y m -> Suc (y m))
{-
 - Reducir una expresión... lo cual no tiene sentido.
 -}

{-
 - Ejercicio 4.23
 - Calcule f y e de la expresión
 -}
-- (<+>) m = foldNat f e
{-
 - usando la propiedad universal. Reduzca la expresión
 -}
-- tres = Suc dos
-- dos <+> tres
--
