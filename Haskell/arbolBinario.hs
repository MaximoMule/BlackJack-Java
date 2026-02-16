data ArbolBin a = Vacio | Nodo a (ArbolBin a) (ArbolBin a ) deriving (Show)
 
addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addTree x Vacio = Nodo x Vacio Vacio
addTree x (Nodo a izq der) 
                         | x == a = Nodo a izq der
                         | x < a = Nodo a(addTree x izq) der
                         | x > a = Nodo a izq (addTree x der)

inOrderTree :: (Ord a) => ArbolBin a -> [a]
inOrderTree Vacio = []
inOrderTree (Nodo x izq der) = inOrderTree izq ++ [x] ++ inOrderTree der

surfTree :: (Ord a) => a -> ArbolBin a -> Bool
surfTree x Vacio =False
surfTree x (Nodo a izq der)
                           | x == a = True
                           | x < a = surfTree x izq
                           | x > a = surfTree x der
