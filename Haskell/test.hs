module Test where

sign :: (Ord a, Num a) => a -> a
sign x
    | x > 0     = 1
    | x < 0     = -1
    | otherwise = 0
