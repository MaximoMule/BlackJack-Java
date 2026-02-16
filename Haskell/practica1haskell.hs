sumDiv n = sum[x | x <- [1..n], n `rem` x == 0, x /= n ]
perfectosn n = [x | x <-[1..n], sumDiv x == x]

juntar :: (Ord a) => [a] -> [a] -> [a]
juntar a [] = a
juntar [] b = b
juntar (x:xs) (y:ys) 
                    | x <= y = x : juntar xs (y:ys)
                    | otherwise = y : juntar (x:xs) ys

particion :: (Ord a) => a -> [a] -> ([a],[a])
particion a [] =([],[])
particion a (x:xs) = if x < a then (x:l1,l2) else (l1,x:l2)
                     where (l1,l2) = particion a xs

qsort :: (Ord a) => [a] -> [a] 
qsort [] = []
qsort [a] = [a]
qsort (x:xs) = qsort lista1 ++ [x] ++ qsort lista2
                where (lista1,lista2) =particion x xs

mizip :: [a] -> [b] -> [(a,b)]
mizip [] _ = []
mizip _ [] = []
mizip (x:xs) (y:ys) = (x,y) : mizip xs ys

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar (x:xs) (y:ys) = sum[x*y | (x,y) <- mizip xs ys]

indexado :: [a] -> [(a,Integer)]
indexado  lista = mizip lista [1..]

inserta :: (Ord a ) => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs) = if a < x then (a:x:xs) else (x: inserta a xs)

split2 :: [a] -> ([a],[a])
split2 [] =([],[])
split2 (x:xs:t) = let (m1,m2) = split2 t
                 in (x:m1,xs:m2) 


data ArbolBin a = Vacio | Nodo a (ArbolBin a ) (ArbolBin a ) deriving (Show)


addNodo :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addNodo x Vacio = Nodo x Vacio Vacio
addNodo x (Nodo a izq der)  
                           | x == a = Nodo a izq der
                           | x < a = Nodo a (addNodo x izq) der
                           | x > a = Nodo a izq (addNodo x der)

surfTreeTree :: (Ord a) => a  -> ArbolBin a -> Bool
surfTreeTree x Vacio = False
surfTree x (Nodo a izq der)
                         | x == a = True
                         | x < a = surfTreeTree x izq
                         | x > a = surfTreeTree x der

inOrderTree :: (Ord a) => ArbolBin a -> [a]
inOrderTree (Nodo a izq der) = inOrderTree izq ++ [a] ++ inOrderTree der

--Funciones para cola almacenada en un arbol

minNodo :: (Ord a) => ArbolBin a -> (a,ArbolBin a)
minNodo Vacio = error "El arbol esta vacio"
minNodo (Nodo a Vacio der) =(a,der)
minNodo (Nodo a izq der) = let(x,newIzq) = minNodo izq
                           in (x,Nodo a newIzq der)

delNodo :: (Ord a) => a -> ArbolBin a -> ArbolBin a
delNodo x Vacio = Vacio
delNodo x (Nodo a izq der) 
                          | x < a = Nodo a (delNodo x izq) der
                          | x > a = Nodo a izq (delNodo x der)
                          | x == a = let (newNodo,newDer) = minNodo der
                                    in Nodo newNodo izq newDer


--Funciones de la cola
newtype ColaPrioridad a = ColaPrio(ArbolBin a) deriving (Show)

mkqpr :: ColaPrioridad a
mkqpr =  ColaPrio Vacio

addqpr :: (Ord a) => a -> ColaPrioridad a -> ColaPrioridad a 
addqpr x (ColaPrio a) = ColaPrio (addNodo x a )

nextqpr :: (Ord a) => ColaPrioridad a -> a
nextqpr (ColaPrio a) = fst (minNodo a)
popqpr (ColaPrio a) = ColaPrio(snd(minNodo a))
