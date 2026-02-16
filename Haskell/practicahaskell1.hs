sumDiv n = sum[x | x <- [1..n], n `rem` x == 0, x /= n]

perfectos n = [x | x <- [1..n], sumDiv x == x]

juntar :: (Ord a) => [a] -> [a] -> [a]
juntar a [] = a
juntar [] b = b
juntar(x:xs)(y:ys)= if x < y then (x: juntar xs (y:ys)) else (y: juntar ys (x:xs))

particion :: (Ord a) => a -> [a] -> ([a],[a])
particion a [] = ([],[])
particion a (x:xs) = if x < a then (x:lista1,lista2) else (lista1,x:lista2)
                    where (lista1,lista2)= particion a xs  

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (x:xs) = qsort lista1 ++ [x] ++ qsort lista2
               where (lista1,lista2)= particion x xs

mizip :: [a] -> [b] -> [(a,b)]
mizip [] _ = []
mizip _ [] = []
mizip (x:xs)(y:ys)= (x,y) : mizip xs ys

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar (x:xs) (y:ys) = sum[x*y | (x,y) <- mizip xs ys]

indexado :: [a] -> [(a,Int)]
indexado lista1 = mizip lista1 [1..]

inserta :: (Ord a) => a -> [a] -> [a]
inserta a [] = [a] 
inserta a (x:x2:t) = if x < x2 then (x:x2:t) else (x2: inserta x t)

split [] = ([],[])
split [a] = ([a],[])
split(x:x2:xs) = (x:lista1,xs:lista2)
                 where (lista1,lista2) = split xs

data ArbolBin a = Vacio | Nodo a (ArbolBin a) (ArbolBin a)  deriving (Show)

mkNewTree :: (Ord a) => ArbolBin  a
mkNewTree = Vacio

addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addTree x Vacio = Nodo x Vacio Vacio
addTree x (Nodo a izq der)
                           | x == a = Nodo a izq der
                           | x < a = Nodo a (addTree x izq) der
                           | x > a = Nodo a izq (addTree x der)

inOrderTree :: (Ord a) => ArbolBin a -> [a]
inOrderTree Vacio = []
inOrderTree (Nodo a izq der) = inOrderTree izq ++ [a] ++ inOrderTree der


minNodo ::  (Ord a) => ArbolBin a -> (a,ArbolBin a)
minNodo Vacio = error "El arbol esta vacio"
minNodo (Nodo a Vacio der) = (a,der)
minNodo (Nodo a izq der) = let (x,newIzq) = minNodo izq
                           in (x,Nodo a newIzq der)

newtype Colaprioridad a = ColaPrio(ArbolBin a ) deriving (Show)
 
 mkqpr :: Colaprioridad a
 mkqpr = ColaPrio Vacio

 addqpr :: (Ord a) => a -> Colaprioridad a -> Colaprioridad a
 addqpr x (Colaprio a ) = ColaPrio(addTree x a)
 nextqpr (ColaPrio a) = fst(minNodo a)
 popqpr (ColaPrio a) =ColaPrio(snd(minNodo a))