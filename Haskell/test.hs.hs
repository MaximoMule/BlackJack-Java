operacion :: Int -> Int -> Int
operacion x y = 
    let resultadoOperacion = x + y
    in
        if resultadoOperacion>1 then "El resultado es mayor a 1"
        else "El resultado es menor a 1"
main :: IO ()
main = do
    let numero = -5
    let mensaje = operacion numero numero
    putStrLn mensaje




