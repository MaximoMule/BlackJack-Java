data ArbolBin a = Vacio | Nodo a (ArbolBin a) (ArbolBin a) deriving (Show)

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
inOrderTree (Nodo a izq der) = inOrderTree izq ++ [a] ++ inOrderTree der 

--Las funciones que vamos a utilizar para la cola son addNodo y inArbol

--Devuelve el elemento minimo del arbol
minNodo :: (Ord a) => ArbolBin a -> (a,ArbolBin a)
minNodo Vacio = error "El arbol esta vacio"
minNodo (Nodo a Vacio der) =(a,der)
minNodo (Nodo a izq der) = let (x,newizq) = minNodo izq
                           in (x,Nodo a newizq der)

--Elimina  elemento del arbol
delNodo :: (Ord a) => a -> ArbolBin a -> ArbolBin a
delNodo x Vacio = Vacio
delNodo x (Nodo a izq der)  | x < a = Nodo a (delNodo x izq) der
                            | x > a = Nodo a izq (delNodo x der)
                            | x == a = let(newRaiz,newDer) = minNodo der
                                        in Nodo newRaiz izq newDer

newtype ColaPrioridad a = ColaPrio(ArbolBin a) deriving (Show)

mkqpr :: ColaPrioridad a
mkqpr =ColaPrio Vacio

addqpr :: (Ord a) => a -> ColaPrioridad a -> ColaPrioridad a 
addqpr x (ColaPrio a) = ColaPrio(addTree x a)

nextqpr :: (Ord a) => ColaPrioridad a -> a 
nextqpr (ColaPrio a) = fst(minNodo a)

popqpr :: (Ord a) => ColaPrioridad a -> ColaPrioridad a 
popqpr (ColaPrio q) = ColaPrio (snd (minNodo q))

