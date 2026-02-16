newtype ColaPrioridad a = CP[a] deriving (Show)

mkqpr :: ColaPrioridad a
mkqpr = CP []
 
addqpr :: Ord a => a -> ColaPrioridad a -> ColaPrioridad a
addqpr x (CP xs) = CP (insertSorted x xs)
  where insertSorted x [] = [x]
        insertSorted x (y:ys)
          | x <= y = (x:y:ys)
          | otherwise = y : insertSorted x ys

nextqpr :: ColaPrioridad a -> a
nextqpr (CP[])= error "Cola vacia"
nextqpr (CP(x:xs))= x 

popqpr :: ColaPrioridad a -> ColaPrioridad a
popqpr (CP[])= error "Cola vacia"
popqpr(CP(x:xs))= CP xs