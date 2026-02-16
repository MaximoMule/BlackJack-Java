module Test where
sign :: (Ord a, Num a) => a -> a
sign x 
      | x>0 = 4
      | x<0 = -1
      |otherwise = 0
absc :: Int -> Int
absc x
    | x >= 0 =x
    | otherwise = -x

pot :: Int -> Int -> Int
pot x y = y * x
 
xoor :: Bool -> Bool -> Bool
xoor True True = False
xoor False False = False
xoor _      _ = True
