sumDivisores n = sum [x | x <- [1..n], n `rem` x == 0, x /= n]
perfectos n = [x | x <- [1..n], sumDivisores x == x ]

juntar :: (Ord a) => [a] -> [a] -> [a]
juntar a [] = a
juntar [] b = b
juntar (x:xs) (y:ys)= if x < y then (x: juntar xs(y:ys)) else (y: juntar ys (x:xs))

particion :: (Ord a) => a -> [a] -> ([a],[a])
particion a [] =([],[])
particion a (x:xs) = if x < a then (x:l1,l2) else (l1,x:l2)
                     where (l1,l2) = particion a xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort(x:xs:resto) = qsort lista1 ++ [x] ++ qsort lista2
                    where(lista1,lista2) = particion x resto

mizip :: [a] -> [b] -> [(a,b)]
mizip _ [] = []
mizip [] _ = []
mizip (x:xs) (y:ys)= (x,y) : mizip xs ys

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar (x:xs) (y:ys) = sum[x*y | (x,y) <- mizip xs ys ]

indexado :: [a] -> [(a,Int)]
indexado lista = mizip lista [1..]

inserta :: (Ord a) => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs) =if a < x then (a:x:xs) else  (x : inserta a xs)

split :: (Ord a ) => [a] -> ([a],[a])
split [] = ([],[])
split (x:xs:resto) = (x:l1,xs:l2)
                     where(l1,l2) = split resto


--Arbol
data ArbolBin a = Vacio | Nodo a (ArbolBin a) (ArbolBin a) deriving (Show)

mkNewtree :: (Ord a) => ArbolBin a
mkNewtree = Vacio
addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addTree x Vacio = Nodo x Vacio Vacio 
addTree x (Nodo a izq der) 
                          | x == a = Nodo a izq der
                          | x < a = Nodo a (addTree x izq) der
                          | x > a = Nodo a izq (addTree x der)

surfTree :: (Ord a) => a -> ArbolBin a -> Bool
surfTree x Vacio = False
surfTree x (Nodo a izq der) 
                           | x == a = True
                           | x < a = surfTree x izq
                           | x > a = surfTree x der

inOrderTree :: (Ord a) => ArbolBin a -> [a]
inOrderTree Vacio = []
inOrderTree (Nodo a izq der) = inOrderTree izq ++ [a] ++ inOrderTree der

newtype ColaPrioridad a = ColaPrio(ArbolBin a) deriving (Show)

minNodo :: (Ord a) => ArbolBin a -> (a,ArbolBin a)
minNodo Vacio = error "El arbol esta vacio"
minNodo (Nodo a Vacio der) = (a,der)
minNodo (Nodo a izq der) =let (x,newIzq)=minNodo izq 
                          in (x,Nodo a newIzq der)

mkqpr :: ColaPrioridad a 
mkqpr = ColaPrio Vacio

addqpr :: (Ord a) => a -> ColaPrioridad a -> ColaPrioridad a
addqpr x (ColaPrio a) = ColaPrio(addTree x a)

nextqpr :: (Ord a) => ColaPrioridad a -> a
nextqpr (ColaPrio a)=fst(minNodo a)

popqpr :: (Ord a) => ColaPrioridad a -> ColaPrioridad a
popqpr (ColaPrio a)=ColaPrio(snd(minNodo a))

--Set


newtype Set a = Set [a] deriving (Show)

emptySet :: Set a
emptySet = Set []

setEmpty :: (Eq a) => Set a -> Bool
setEmpty (Set []) = True
setEmpty (Set [a]) = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet a (Set []) = False
inSet a (Set(x:xs)) = a == x || inSet a (Set (xs))

addSet :: (Eq a) => a -> Set a -> Set a
addSet x (Set xs) = if inSet x (Set xs) then Set xs else Set (x:xs)

delSet :: (Eq a) => a -> Set a -> Set a
delSet _ (Set []) = Set []
delSet a (Set (x:xs)) = if x /= a then addSet x (delSet a (Set xs)) else delSet a (Set xs)

unionSet :: (Eq a) => Set a -> Set a -> Set a 
unionSet (Set []) set2 = set2
unionSet (Set (x:xs)) set2 = if inSet x set2 then unionSet (Set xs) set2 else unionSet (Set xs) (addSet x set2)
