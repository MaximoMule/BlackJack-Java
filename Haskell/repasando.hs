borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = [x] ++ borrarUltimo xs

serie []= [[]]
serie xs = serie (init xs) ++ [xs]

paresIguales  a b c d 
                     | a == b && c == d = True
                     | a == c && b == d = True
                     | a == d && b == c = True
                     |otherwise = False

isosceles a b c 
                | a == b || a == c = True
                | b == c = True
                | otherwise =  False

ror [] _ = []
ror xs n = if n < length xs then ror' xs n else error "No se puede "

ror' xs 0 = xs
ror' (x:xs) n = ror' (xs++[x]) (n-1)

upto n m | n <= m = n : upto (n+1) m 
         |otherwise = []

eco [] = []
eco xs = eco' xs 1

eco' [] _ = []
eco' (x:xs) n = replicate  n x ++ eco' xs (n+1)

cambios [] = []
cambios xs = [i |(x,y,i) <- zip3 (xs) (tail xs) [0..],x/=y] 

oblongos = [x*y | (x,y) <- zip [0..50] [1..50]]

abundante = [x | x <- [1..100],sum [y | y <- [1..(x `div` 2)], x `mod` y  == 0] > x]

data Color = Color Int Int Int deriving (Show)

mezclar :: Color -> Color -> Color 
mezclar (Color r1 g1 b1) (Color r2  g2 b2) = Color ((r1 + r2 ) `div` 2) ((g1 + g2 ) `div` 2) ((b1 + b2 ) `div` 2)

data Linea = Linea [Char] Int deriving (Show)

vacia :: Linea
vacia = Linea [] 0

insertar ::  Char  -> Linea -> Linea
insertar c (Linea cont p) = Linea(take p cont ++ [c] ++ drop p cont) (p+1)


eliminar :: Linea -> Linea
eliminar (Linea [x] _)=Linea [] 0
eliminar (Linea cont p)= Linea (init cont) (p-1)

data Clist a = EmptyCl  | Cunit a | Consnoc a (Clist a)  a  deriving (Show)
--Head devuelve primer elementod e la Clist
headCl (Cunit a) = a 
headCl (Consnoc a _ _) = a 

tailCL EmptyCl = error "tailCL: Lista vacía"
tailCL (Cunit x) = EmptyCl
tailCL (Consnoc x EmptyCl r) = Cunit r
tailCL (Consnoc x (Cunit y) r) = Consnoc y EmptyCl r
tailCL (Consnoc x ys r)= Consnoc (headCl ys) (tailCL ys) r

isEmptyCl EmptyCl = True
isEmptyCl (Cunit x) =False
isEmptyCl (Consnoc x ys r)=False

isCunit EmptyCl =False
isCunit (Cunit x) = True
isCunit (Consnoc x xs r)=False

reverseCl EmptyCl = EmptyCl
reverseCl (Cunit x) = Cunit x 
reverseCl (Consnoc x EmptyCl r) = Consnoc r EmptyCl x 
reverseCl (Consnoc x xs r)= Consnoc r (reverseCl xs) x 

data Tree a = Empty | Nodo (Tree a) a (Tree a)  deriving (Show)

calcular :: Tree a -> Int
calcular Empty = 0
calcular (Nodo izq _ der) = 1 + calcular izq + calcular der

insertar2 :: a -> Tree a -> Tree a 
insertar2 x Empty = Nodo Empty x Empty
insertar2 x (Nodo izq a der)
                        | calcular izq <= calcular der = Nodo (insertar2 x izq) a der
                        | otherwise = Nodo izq a (insertar2 x der)


minNodo :: Tree a -> (a,Tree a)
minNodo (Nodo Empty a der) =(a,der)
minNodo (Nodo izq a der) = let(x,newIzq) = minNodo izq
                           in (x,Nodo newIzq a der)
 
eliminar2 :: (Ord a) => a -> Tree a -> Tree a
eliminar2 _ Empty = Empty
eliminar2 x (Nodo izq a der)
    | x < a = Nodo (eliminar2 x izq) a der
    | x > a = Nodo izq a (eliminar2 x der)
    | x == a = case der of
        Empty -> izq
        _ -> let (newNodo, newDer) = minNodo der
             in Nodo izq newNodo newDer

fibonacci 1 = 1 
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

sumatoria1 n= fibonacci(2*n+1) - fibonacci(2*n-1)

sumatoria2 n= (fibonacci(2*n+2)-1) - fibonacci(2*n)

sumatoria3 a b n = a*(n+1) + b * (n*(n+1)/2)