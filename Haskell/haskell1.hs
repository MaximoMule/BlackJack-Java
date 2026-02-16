--Funciones Basicas
--Doble de un numero

doble :: Int -> Int
doble x = x + x

--Doble y resto un x
dobleResto :: Int -> Int -> Int
dobleResto x y = doble x - y

--Sumar elementos de una lista
sumaLista :: [Int]-> Int
sumaLista [] = 0
sumaLista (x:xs)= x + sumaLista xs

--Desde cero hasta n
desdeCero :: Int -> [Int]
desdeCero n = [0..n]

--Funcion esPar
esPar :: Int -> Bool
esPar n = n `rem` 2 == 0

--Funcion abs
absMio :: Int -> Int
absMio n = if n > 0 then n else -n

--Funcion area
area :: Int -> Int 
area x = x * x 

--Longitud de una lista
longitud :: [Int]->Int
longitud[]=0
longitud(x:xs) = 1 + longitud xs

--Primer elemento de la lista
first :: [Int]->Int
first(x:_) = x

--Devuelve el num mayor entre 3
mayor3 :: Int -> Int -> Int -> Int
mayor3 a b c 
              | a > b && a > c = a
              | b > a && b > c = b
              | otherwise = c

--Maximo de una lista recursivo
maxRecursivo :: Ord a => [a] -> a
maxRecursivo[x] = x
maxRecursivo (x:xs) = max x (maxRecursivo xs)

--Maximo facil
maxFacil :: Ord a => [a] -> a
maxFacil = maximum

--Minimo de una lista recursivo
minRecursivo :: Ord  a => [a] -> a
minRecursivo[x] = x
minRecursivo(x:xs) = min x (minRecursivo xs)

--Minimo facil
minFacil :: Ord a => [a] -> a
minFacil=minimum

swwap :: (a,b) -> (b,a)
swwap (a,b)=(b,a)

practica :: Int -> Int
practica x = x * 3

funcion1 :: (Int -> Int) -> Int -> Int
funcion1 practica y = practica y 

funcion2 :: Int -> (Int -> Int)
funcion2 x = \ y -> x * y

aplicar :: Int -> Int
aplicar = funcion2 4

funcion3 :: (Int -> Int) -> (Int -> Int)
funcion3 aplicar3 = aplicar3

aplicar3 :: (Int -> Int)
aplicar3 x = x * 3 

esMayor :: Int -> Bool
esMayor x = if x >= 0 then True else False

esVocal :: Char -> Bool
esVocal a = if (a `elem` "aeio") then True else False

parVocal :: (Int , Char) -> Bool
parVocal (a,b) = if (a `rem` 2 == 0) && (b `elem` "aeiou") then True else False

longitudLista :: [a] -> Int
longitudLista[] = 0
longitudLista(_:xs) = 1 + longitudLista xs

sumaDeElementos :: Num a => [a] -> a
sumaDeElementos [] = 0
sumaDeElementos (x:xs) = x +  sumaDeElementos xs

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

divisores2 :: Int -> [Int]
divisores2 0 = []
divisores2 n = f[1..n]
             where
              f[]=[]
              f(x:xs) =if (n `mod` x == 0)then x: f (xs) else f xs 

ordenarLista :: Ord a => [a] -> [a]
ordenarLista[]=[]
ordenarLista(x:xs) =
    let menores = ordenarLista [a | a <- xs , a <= x]
        mayores = ordenarLista [a | a <- xs , a > x]
        in menores ++ [x] ++ mayores

esPalindromo :: String -> Bool
esPalindromo cadena = cadena == reverse cadena

divisores :: Int -> [Int]
divisores 0 = []
divisores n = f[1..n]
             where
              f[] = []
              f(x:xs)=if(n `mod` x == 0) then x: (f xs) else f xs
              
primos n = [ x | x<- [1..n], cuenta (divisores x) == 2]

cuenta [] = 0
cuenta (x:xs) = 1 + cuenta xs

prime n = divisores n == [1,n]
primes m = [x | x <- [1..m],prime x]

mizip [] _ = []
mizip _ [] =[]
mizip (x:xs) (y:ys) = (x,y) : mizip xs ys

addya [] =[]
addya (x:xs) = mizip (x:xs) xs

addy y = zip y (tail y)

compro t =[x <= y | (x,y) <- addy t]

miAnd [] = True
miAnd (x:xs) = x && miAnd xs

borrar y [] = []
borrar y lista =[x | x <- lista, x /= y]

ordenarSort [] = []
ordenarSort lista = m : ordenarSort lista'
                   where 
                    m = minimum lista
                    lista' =borrar m lista

insertarSort x [] = [x]
insertarSort x (y:r) = if x < y then (x:y:r)
                       else y : insertarSort x r 

ordenarSort2 [] = []
ordenarSort2 [x] = [x]
ordenarSort2 (x:xs) = insertarSort x (ordenarSort2 xs)

qsort [] = []
qsort[x] = [x]
qsort(x:xs) = qsort(menores)  ++ [x] ++ qsort(mayores)
              where
                menores =[y | y <- xs , y<x]
                mayores =[y | y <- xs ,y>x]
                
split[]=([],[])
split[x]=([x],[])
split(x:y:resto)=(x:impares,y:pares)
                where 
                  (impares,pares)=split resto

merge [] [] = []
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys) = if x<y then x: (merge xs (y:ys))
                      else y: (merge ys(x:xs))

msort [] = []
msort [x] = [x]
msort (l) = let
            (m1,m2) = split l
            m1p = msort m1;
            m2p = msort m2
            in
              merge m1p m2p

