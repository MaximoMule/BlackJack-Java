import Data.Char (isDigit)
import Data.List (transpose)
import System.IO

type Tablero = [[Char]]
data Jugador = X | O deriving (Eq, Show)

-- Función principal
main :: IO ()
main = jugar

jugar :: IO ()
jugar = do
    putStrLn "Ingrese el número de filas (n) (debe ser >= 3):"
    n <- getIntInput
    putStrLn "Ingrese el número de columnas (m) (debe ser >= 3):"
    m <- getIntInput
    let tablero = replicate n (replicate m ' ')  -- Inicializa el tablero vacío
    putStrLn "El juego ha comenzado." -- Línea de prueba
    loop tablero X

-- Función que controla el flujo del juego
loop :: Tablero -> Jugador -> IO ()
loop tablero jugador = do
    imprimirTablero tablero
    putStrLn $ "Turno del jugador " ++ show jugador ++ ". Ingrese las coordenadas de la jugada."
    (fila, columna) <- obtenerCoordenadas
    if esJugadaValida tablero fila columna
        then do
            let nuevoTablero = actualizarTablero tablero fila columna (simbolo jugador)
            if hayGanador nuevoTablero (simbolo jugador)
                then do
                    imprimirTablero nuevoTablero
                    putStrLn $ "¡El jugador " ++ show jugador ++ " ha ganado!"
                    preguntarNuevaPartida
                else loop nuevoTablero (cambiarJugador jugador)
        else do
            putStrLn "Jugada inválida, intente de nuevo."
            loop tablero jugador

-- Pregunta si desea jugar de nuevo
preguntarNuevaPartida :: IO ()
preguntarNuevaPartida = do
    putStrLn "¿Desea jugar otra partida? (s/n)"
    respuesta <- getLine
    if respuesta == "s" then jugar else putStrLn "Gracias por jugar."

-- Cambia al jugador siguiente
cambiarJugador :: Jugador -> Jugador
cambiarJugador X = O
cambiarJugador O = X

-- Retorna el símbolo del jugador como 'x' o 'o'
simbolo :: Jugador -> Char
simbolo X = 'x'
simbolo O = 'o'

-- Imprime el tablero actual
imprimirTablero :: Tablero -> IO ()
imprimirTablero tablero = do
    let n = length tablero
    let m = length (head tablero)
    putStr "    "
    mapM_ (\i -> putStr (show i ++ "     ")) [1..m]
    putStrLn ""
    putStrLn (replicate (6 * m + 3) '-')
    mapM_ (\(i, fila) -> imprimirFila i fila) (zip [1..] tablero)

imprimirFila :: Int -> [Char] -> IO ()
imprimirFila i fila = do
    putStr (show i ++ ":   ")
    mapM_ (\c -> putStr ("|  " ++ [c] ++ "  ")) fila
    putStrLn "|"
    putStrLn (replicate (6 * (length fila) + 3) '-')

-- Verifica si la jugada es válida (dentro de los límites y casilla vacía)
esJugadaValida :: Tablero -> Int -> Int -> Bool
esJugadaValida tablero fila columna =
    fila >= 1 && fila <= length tablero &&
    columna >= 1 && columna <= length (head tablero) &&
    (tablero !! (fila - 1) !! (columna - 1) == ' ')

-- Actualiza el tablero con la nueva jugada
actualizarTablero :: Tablero -> Int -> Int -> Char -> Tablero
actualizarTablero tablero fila columna simbolo =
    take (fila - 1) tablero ++
    [take (columna - 1) (tablero !! (fila - 1)) ++ [simbolo] ++ drop columna (tablero !! (fila - 1))] ++
    drop fila tablero

-- Verifica si un jugador ha ganado
hayGanador :: Tablero -> Char -> Bool
hayGanador tablero simbolo = any (verificarLinea simbolo) (filas ++ columnas ++ diagonales)
  where
    filas = tablero
    columnas = transpose tablero
    diagonales = diagonalesPrincipales tablero ++ diagonalesPrincipales (map reverse tablero)

-- Verifica si hay 3 en línea en una lista de celdas
verificarLinea :: Char -> [Char] -> Bool
verificarLinea simbolo linea = replicate 3 simbolo `elem` consecutivos 3 linea

-- Obtiene las diagonales principales del tablero
diagonalesPrincipales :: Tablero -> [[Char]]
diagonalesPrincipales [] = []
diagonalesPrincipales (f:fs) = zipWith (:) f ([]:diagonalesPrincipales fs)

-- Obtiene subconjuntos consecutivos de una lista
consecutivos :: Int -> [a] -> [[a]]
consecutivos n xs = [take n (drop i xs) | i <- [0..length xs - n]]

-- Solicita y valida las coordenadas ingresadas por el jugador
obtenerCoordenadas :: IO (Int, Int)
obtenerCoordenadas = do
    putStr "Fila: "
    fila <- getIntInput
    putStr "Columna: "
    columna <- getIntInput
    return (fila, columna)

getIntInput :: IO Int
getIntInput = do
    input <- getLine
    if esEntero input
        then
            let n = read input :: Int
            in if n >= 1
                   then return n
                   else do
                       putStrLn "El número debe ser mayor o igual a 1. Inténtelo de nuevo."
                       getIntInput
        else do
            putStrLn "Entrada inválida. Por favor, ingrese un número entero."
            getIntInput

-- Verifica si una cadena representa un número entero positivo
esEntero :: String -> Bool
esEntero "" = False
esEntero xs = all isDigit xs
