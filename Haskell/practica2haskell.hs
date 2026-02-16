double :: Int -> Int
double x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = x * 2 + y * 2

menor100 :: Int -> Int
menor100 x = if x < 100
             then x * 2
             else x

boomBangs :: [Int] -> [String]
boomBangs (x:xs) = [if x < 10 then "BOOM!" else "BANG!" | x <- xs , odd x]

miZip :: [a] -> [b] -> [(a,b)]
miZip [] _ = []
miZip _ [] = []
miZip (x:xs) (y:ys) = (x,y ) : miZip xs ys

unique' mesa [] = mesa
unique' mesa (x:xs) =
  if x `elem` mesa
    then unique' mesa xs
    else unique' (mesa++[x]) xs


unique xs = [e | (e,i) <- zip xs [1..], not (elem e (drop i xs)) ]

prodEscalar xs ys= sum [x*y | (x,y) <- zip xs ys]

suma[] =0
suma(x:xs) = x + suma xs

alguno [] = False
alguno (x:xs) = x || alguno xs

five x = 5

first :: (a,b) -> a 
first(x,_) = x 

sign :: Int -> Int 
sign x  
      | x<0  = -1
      | x>0 = 1
      | x==0 = 0

pot :: Int -> Int -> Int
pot x y = y ^ x

max3 :: Int -> Int -> Int -> Int 
max3 a b c 
          | a > b && a > c = a 
          | b > a && b > c = b 
          | c > a && c > b = c

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)


borrarUltimo :: [Int] -> [Int]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = [x] ++ borrarUltimo xs

serie :: [Int] -> [[Int]]
serie [] = [[]]
serie xs = serie' xs

serie' :: [Int] -> [[Int]]
serie' [] = [[]]
serie' xs = serie' (init xs ) ++ [xs]
--1 1 3 3 True
--1 3 3 1 True

paresIguales :: Int -> Int -> Int -> Int -> Bool 
paresIguales x y j z  
                    | x == y && j == z = True
                    | x == j && y == z = True 
                    | x == z && y == j = True
                    | otherwise = False

isosceles :: Int -> Int -> Int -> Bool
isosceles x y z   
                | x == y || x == z = True
                | z == y || z == x = True
                |otherwise = False 

ror :: Int -> [Int] -> [Int]
ror _ [] = error "Lista vacia"
ror n xs 
         | n < 0 || n > length xs = error "N debe ser mayor a 0 y menor a la longitud"  
         |otherwise = ror' n xs 

ror' :: Int -> [Int] -> [Int]
ror' 0 xs = xs
ror' n (x:xs) = ror' (n-1) (xs ++ [x])

upto :: Int -> Int -> [Int]
upto n m 
        | n > m = []
        | n <= m = [n] ++ upto (n+1) m 

eco :: String -> String
eco xs = eco' xs 1
 
eco' :: String -> Int -> String
eco' [] _ = []
eco' _ 0 =[]
eco' (x:xs) n= replicate n x ++ eco' xs (n+1)

cambios :: Eq a => [a] -> [Int]
cambios xs = [i | (x , y , i) <- zip3 xs (tail xs) [0..], x /= y]

oblongo :: [Int]
oblongo = [x*(x+1) | x <- [0..10]]

abundante :: [Integer]
abundante = [x | x <- [1..100],sum[y | y <- [1..(x `div` 2)], x `mod` y == 0] > x ]

euler :: Int -> Int
euler n = sum [x | x <- [1..(n-1)], x `mod` 3 == 0 || x `mod` 5 == 0  ]

expandir :: [Int] -> [Int] 
expandir [] = []
expandir xs = [x | x <- xs , _ <- [1..x]]
-- [3,2] = [3,3,3] <- [1..3] esto se repite 3 veces

-- recibe una funcion que recibe un entero y devuelve un entero
--y devuelve una funcion que recibe un booleano y devuelve otro

f :: Int -> Int 
f x = x * x 

b :: Bool -> Bool
b True = False
b False = True 

funcion1 :: (Int -> Int ) -> (Bool -> Bool)
funcion1 f b = if f 5 < 0 then b else b  

funcion1_2 :: (Int -> Int) -> (Bool -> Bool)
funcion1_2 f b 
              | f 5 == 0 = b 
              | f 5 < 0  = b 

--Funcion que recibe Bool -> (Int -> Bool)

fun :: Int -> Bool
fun x = if x == 0 then True else False

funcion2_2 :: Bool -> (Int -> Bool)
funcion2_2 True _ = fun 2
funcion2_2 False _ = fun 0

fun2:: Int -> Bool
fun2 x = if  x > 0 then True else False

funcion2_22 :: Bool -> (Int -> Bool)
funcion2_22 True _ = fun2 2
funcion2_22 False _= fun2 (-1)

funcionChar :: Char -> Char 
funcionChar a = a

funcionChar2 :: Char -> Char
funcionChar2 a = if 'a' `elem` "aeiou" then 'V' else 'F' 

f3 :: Int -> Bool
f3 x = if x > 0 then True else False

funcion3_1 :: Int -> (Int -> Bool) -> [Int]
funcion3_1 x f3 = if f3 x then [x] else []

funcion3_2 :: Int -> (Int -> Bool) -> [Int]
funcion3_2 x f3 = if f3 x then [0..x] else []

 
funcion4_1 :: [a] -> (a ->[b]) ->[b]
funcion4_1 [] _ = []
funcion4_1 (x:xs) f = f x 

funcion5_1 :: [[a]] -> (a -> Bool) -> [a]
funcion5_1 lista f = concatMap (filter f) lista -- Esta funcion va a tomar una lista de listas y una funcion f
-- va a filtrar todos los elementos que cumplan la condicion de la funcion f
--es decir cuando devuelva TRUE la funcion los incluye sino no , y luego los concatena todos en una lista

funcio6_1 :: (a,b,c) -> Bool
funcio6_1 (a,b,c) = (filter f) (a,b,c)