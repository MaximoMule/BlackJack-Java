--Ejercicio 1 a
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

--Ejercicio c
serie [] =[[]]
serie (x:xs) = serie (init xs) ++ xs

--Ejercicio  d 
paresIguales a b c d 
                    | a == b && c==d = True
                    | a == c && b==d = True
                    | a == d && b==c =True
                    |otherwise = False

--Ejercicio e
isosceles a b c 
               |a == b || a == c || b == c = True
               |otherwise = False

--Ejercicio f
ror n [] =[]
ror 0 xs = xs
ror n (x:xs) = ror (n-1) (xs ++ [x])

--Ejercicio g
upto n m
        |n > m = []
        |n <= m = [n] ++ upto (n+1) m

--Ejercicio h
eco _ [] = []
eco n (x:xs) = replicate n x ++ eco (n+1) xs

--Ejecicio 2 a
cambios xs = [i | (x,y,i) <- zip3 (xs) (tail xs) [0..], x/=y]

--Ejercicio 2 b
oblongoNumber :: [Int]
oblongoNumber = [(x*y) | (x,y) <- zip [0..20] [1..20]]

--Ejercicio c 
abundante =[x | x  <- [1..30],sum[y | y <-[1..(x`div`2)], x`mod`y == 0] > x]

--Ejercicio e
euler n =sum[x | x <- [1..(n-1)], x `mod` 5 == 0 || x `mod` 3 == 0]

--Ejercicio f

expandir []=[]
expandir (x:xs)= replicate x x ++ expandir xs

--EJERCICIOS 2 

--Ejercicio a
f1 :: Int->Int
f1 x = x + x

f2 :: Bool->Bool
f2 True = False
f2 False = True

fPrincipal :: (Int->Int) -> (Bool->Bool)
fPrincipal f1 = f2 

--Ejercicio b
f3 :: Int->Bool
f3 0 = True
f3 1 = False

fPrincipal2 :: Bool -> (Int->Bool)
fPrincipal2 True = f3 
fPrincipal2 False = f3 

--Ejercicio c
fPrincipal3 :: Char -> Char
fPrincipal3 x = if x `elem` "aeiou" then 'V' else 'F'

--Ejercicio d
f'3 :: Int->Bool
f'3 0 = True
f'3 _ = False
fPrincipal4 :: Int -> (Int -> Bool) -> [Int]
fPrincipal4 x f'3 = if f'3 x then [x] else [0..x]

--Ejecicio E
f4 :: Int->[Int]
f4 x =[x,x]

fPrincipal5 :: [a] -> (a -> [b]) -> [b]
fPrincipal5 xs f = concatMap f xs 

--Ejercicio f 

f5 :: Int -> Bool
f5 x = if x > 0 then True else False

fPrincipal6 :: [[a]] -> (a ->Bool) ->[a]
fPrincipal6 xs f = filter f (concat xs)

--Ejercicio G
--EJERCICIOS 5--
--Ejercicio a--
map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []
--Ejercicio b--
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr(\x acc -> if f x then x : acc else acc) []

--Ejercicio c-- 
unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldr (\(x,y) (xs,ys) -> (x:xs , y:ys))([],[])

--Ejercicio d --
--pair2List :: (a,[b]) -> [(a,b)]
--pair2List (x,xs)=foldr(\y acc -> (x,y) :acc)[] 

--Ejercicio e

maxSec :: [(Int, Int)] -> (Int, Int)
maxSec [] = error "Lista vacía"
maxSec [x] = x
maxSec xs = foldr(\(a1,a2) (b1,b2) -> if (a2-a1) > (b2-b1) then (a1,a2) else (b1,b2)) (head xs) (tail xs)

------------------------------------PRACTICA 2--------------------------------------------
--Ejercicio 1--
data Color = Color Int Int Int

mezclar :: Color  -> Color  -> Color 
mezclar  (Color r1 g1 b1)  (Color r2 g2 b2) = Color ((r1+r2)`div`2) ((g1+g2)`div`2) ((b1+b2)`div`2) 

--Ejercicio 2-- 
data Linea = Linea ([Char],Int) deriving (Show)

vacia :: Linea
vacia = (Linea([],0)) 

insertar :: Char -> Linea -> Linea
insertar c (Linea([],p)) = Linea([c],(p+1))
insertar c (Linea(xs,p)) = (Linea((take p xs ++ [c] ++ drop p xs),(p+1)))

moverIzq :: Linea -> Linea
moverIzq (Linea (xs,p)) 
                       |p > 0 = (Linea (xs,(p-1)))
                       |otherwise = (Linea (xs,(p)))

moverDer :: Linea -> Linea
moverDer (Linea (xs,p)) = (Linea(xs,p+1))

borrar :: Linea -> Linea
borrar (Linea(xs,p)) = (Linea (take (p-1) xs ++ drop p xs,(p-1)))

--Ejecicio 2 

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)
--HeadCL devuelve la cabeza
headCl :: CList  a -> a
headCl (CUnit a) = a
headCl (Consnoc a _ _) = a

--TailCL devuelve la cola
tailCL :: CList a -> CList a
tailCL (CUnit a) = EmptyCL
tailCL (Consnoc _ EmptyCL x) = CUnit x 
tailCL (Consnoc _ (CUnit x) r)   = Consnoc x (EmptyCL) r
tailCL (Consnoc _ (Consnoc x y ys) r ) = Consnoc x (tailCL(Consnoc x y ys)) r
  

--True si esta vacia , falsa lo contrario
isEmptyCL :: CList a -> Bool
isEmptyCL  EmptyCL = True
isEmptyCL    _     =        False

--True si es un unico elemento
isCUnit :: CList a-> Bool
isCUnit (CUnit x ) = True
isCUnit _          = False

--Reverse clist
reverseCL :: CList a -> CList a 
reverseCL EmptyCL = EmptyCL
reverseCL (Consnoc x(EmptyCL) r ) = Consnoc r (EmptyCL) r 
reverseCL (Consnoc  x (CUnit y) r) = Consnoc r (CUnit y) x
reverseCL (Consnoc x (Consnoc y ys z) r) = Consnoc r (reverseCL (Consnoc y ys z)) x

--InitsCL
initCL EmptyCL = EmptyCL
initCL (CUnit x) = CUnit (CUnit x)

-- Definición del tipo de datos Aexp
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

-- Definición de la función eval
eval :: Aexp -> Int
eval (Num x) = x
eval (Prod n1 n2) = eval n1 * eval n2
eval (Div n1 n2) 
                |eval n2 == 0 = error "division por cero"
                |otherwise = eval n1 `div` eval n2


--Practica 3 METODOS DE TRABAJO
f :: Int -> Int
f 1 = 1
f 2 = 1  
f n = f (n-1) + f (n-2)

formula1 n = f(2*n+1) - f(2*n-1)
formula2 n =   (f(2*n+2) - f(2*n))-1