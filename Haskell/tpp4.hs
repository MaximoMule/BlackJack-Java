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
    n <- getDimensionInput
    putStrLn "Ingrese el número de columnas (m) (debe ser >= 3):"
    m <- getDimensionInput
    let tablero = replicate n (replicate m ' ')  -- Inicializa el tablero vacío
    putStrLn "El juego ha comenzado."
    loop tablero X 0 0  -- Contador de fichas inicializado en 0 para ambos jugadores

-- Control del flujo del juego
loop :: Tablero -> Jugador -> Int -> Int -> IO ()
loop tablero jugador fichasX fichasO = do
    imprimirTablero tablero
    putStrLn $ "Turno del jugador " ++ show jugador ++ "."
    if (jugador == X && fichasX < 3) || (jugador == O && fichasO < 3)
        then do
            putStrLn "Ingrese las coordenadas para colocar su ficha."
            (fila, columna) <- obtenerCoordenadas
            if esJugadaValida tablero fila columna
                then do
                    let nuevoTablero = actualizarTablero tablero fila columna (simbolo jugador)
                    let nuevasFichasX = if jugador == X then fichasX + 1 else fichasX
                    let nuevasFichasO = if jugador == O then fichasO + 1 else fichasO
                    if hayGanador nuevoTablero (simbolo jugador)
                        then do
                            imprimirTablero nuevoTablero
                            putStrLn $ "¡El jugador " ++ show jugador ++ " ha ganado!"
                            preguntarNuevaPartida
                        else loop nuevoTablero (cambiarJugador jugador) nuevasFichasX nuevasFichasO
                else do
                    putStrLn "Jugada inválida, intente de nuevo."
                    loop tablero jugador fichasX fichasO
        else do
            putStrLn "Las 3 fichas están en el tablero. Seleccione una ficha para mover."
            putStrLn "Ingrese las coordenadas de la ficha que desea mover:"
            (origenFila, origenColumna) <- obtenerCoordenadas
            if esFichaPropia tablero origenFila origenColumna (simbolo jugador)
                then do
                    putStrLn "Ingrese las coordenadas de la nueva posición:"
                    (destinoFila, destinoColumna) <- obtenerCoordenadas
                    if esJugadaValida tablero destinoFila destinoColumna
                        then do
                            let tableroSinFicha = actualizarTablero tablero origenFila origenColumna ' '
                            let nuevoTablero = actualizarTablero tableroSinFicha destinoFila destinoColumna (simbolo jugador)
                            if hayGanador nuevoTablero (simbolo jugador)
                                then do
                                    imprimirTablero nuevoTablero
                                    putStrLn $ "¡El jugador " ++ show jugador ++ " ha ganado!"
                                    preguntarNuevaPartida
                                else loop nuevoTablero (cambiarJugador jugador) fichasX fichasO
                        else do
                            putStrLn "Movimiento inválido. La nueva posición debe estar vacía."
                            loop tablero jugador fichasX fichasO
                else do
                    putStrLn "La coordenada de origen no contiene su ficha. Intente de nuevo."
                    loop tablero jugador fichasX fichasO

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

-- Verifica si la coordenada contiene la ficha del jugador
esFichaPropia :: Tablero -> Int -> Int -> Char -> Bool
esFichaPropia tablero fila columna simbolo =
    fila >= 1 && fila <= length tablero &&
    columna >= 1 && columna <= length (head tablero) &&
    (tablero !! (fila - 1) !! (columna - 1) == simbolo)

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
    fila <- getCoordinateInput
    putStr "Columna: "
    columna <- getCoordinateInput
    return (fila, columna)

-- Obtiene un número entero >= 3 de la entrada (para dimensiones del tablero)
getDimensionInput :: IO Int
getDimensionInput = do
    input <- getLine
    if esEntero input
        then
            let n = read input :: Int
            in if n >= 3
                   then return n
                   else do
                       putStrLn "El número debe ser mayor o igual a 3. Inténtelo de nuevo."
                       getDimensionInput
        else do
            putStrLn "Entrada inválida. Por favor, ingrese un número entero."
            getDimensionInput

-- Obtiene un número entero >= 1 para coordenadas de jugadas
getCoordinateInput :: IO Int
getCoordinateInput = do
    input <- getLine
    if esEntero input
        then
            let n = read input :: Int
            in if n >= 1
                   then return n
                   else do
                       putStrLn "El número debe ser mayor o igual a 1. Inténtelo de nuevo."
                       getCoordinateInput
        else do
            putStrLn "Entrada inválida. Por favor, ingrese un número entero."
            getCoordinateInput

-- Verifica si una cadena representa un número entero positivo
esEntero :: String -> Bool
esEntero "" = False
esEntero xs = all isDigit xs
