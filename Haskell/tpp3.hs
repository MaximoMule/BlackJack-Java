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
    loop tablero X [] []  -- Comienza con el jugador X y sin fichas colocadas para ninguno

-- Función que controla el flujo del juego
loop :: Tablero -> Jugador -> [(Int, Int)] -> [(Int, Int)] -> IO ()
loop tablero jugador fichasX fichasO = do
    imprimirTablero tablero
    putStrLn $ "Turno del jugador " ++ show jugador ++ ". Ingrese las coordenadas de la jugada."
    
    -- Si el jugador ya tiene 3 fichas, permitirle moverlas
    if length (fichasJugador jugador fichasX fichasO) >= 3
        then do
            putStrLn "Ya tienes 3 fichas colocadas. ¿Deseas mover alguna ficha? (s/n)"
            respuesta <- getLine
            if respuesta == "s"
                then moverFicha jugador tablero fichasX fichasO
                else do
                    putStrLn "Seleccione una casilla para colocar una ficha."
                    (fila, columna) <- obtenerCoordenadas
                    if esJugadaValida tablero fila columna
                        then do
                            let nuevoTablero = actualizarTablero tablero fila columna (simbolo jugador)
                            let nuevoFichas = agregarFicha jugador fila columna fichasX fichasO
                            if hayGanador nuevoTablero (simbolo jugador)
                                then do
                                    imprimirTablero nuevoTablero
                                    putStrLn $ "¡El jugador " ++ show jugador ++ " ha ganado!"
                                    preguntarNuevaPartida
                                else loop nuevoTablero (cambiarJugador jugador) nuevoFichas
                        else do
                            putStrLn "Jugada inválida, intente de nuevo."
                            loop tablero jugador fichasX fichasO
        else do
            (fila, columna) <- obtenerCoordenadas
            if esJugadaValida tablero fila columna
                then do
                    let nuevoTablero = actualizarTablero tablero fila columna (simbolo jugador)
                    let nuevoFichas = agregarFicha jugador fila columna fichasX fichasO
                    if hayGanador nuevoTablero (simbolo jugador)
                        then do
                            imprimirTablero nuevoTablero
                            putStrLn $ "¡El jugador " ++ show jugador ++ " ha ganado!"
                            preguntarNuevaPartida
                        else loop nuevoTablero (cambiarJugador jugador) nuevoFichas
                else do
                    putStrLn "Jugada inválida, intente de nuevo."
                    loop tablero jugador fichasX fichasO

-- Pregunta si desea jugar otra partida
preguntarNuevaPartida :: IO ()
preguntarNuevaPartida = do
    putStrLn "¿Desea jugar otra partida? (s/n)"
    respuesta <- getLine
    if respuesta == "s" then jugar else putStrLn "Gracias por jugar."

-- Cambia al jugador siguiente
cambiarJugador :: Jugador -> Jugador
cambiarJugador X = O
cambiarJugador O = X

-- Retorna el símbolo del jugador como 'X' o 'O'
simbolo :: Jugador -> Char
simbolo X = 'X'
simbolo O = 'O'

-- Obtiene las coordenadas de las jugadas del jugador
obtenerCoordenadas :: IO (Int, Int)
obtenerCoordenadas = do
    putStr "Fila: "
    fila <- getCoordinateInput  -- Obtener la fila (valor entero >= 1)
    putStr "Columna: "
    columna <- getCoordinateInput  -- Obtener la columna (valor entero >= 1)
    return (fila, columna)

-- Agrega una ficha a la lista de fichas del jugador
agregarFicha :: Jugador -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
agregarFicha X fila columna fichasX fichasO = (fila, columna) : fichasX
agregarFicha O fila columna fichasX fichasO = (fila, columna) : fichasO

-- Verifica qué fichas tiene un jugador
fichasJugador :: Jugador -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
fichasJugador X fichasX _ = fichasX
fichasJugador O _ fichasO = fichasO

-- Permite mover una ficha del jugador
moverFicha :: Jugador -> Tablero -> [(Int, Int)] -> [(Int, Int)] -> IO ()
moverFicha jugador tablero fichasX fichasO = do
    let fichas = fichasJugador jugador fichasX fichasO
    putStrLn "Seleccione la ficha que desea mover (número de ficha):"
    mapM_ (\(i, (fila, columna)) -> putStrLn $ show i ++ ". Ficha en (" ++ show fila ++ ", " ++ show columna ++ ")") (zip [1..] fichas)
    seleccion <- getCoordinateInput
    let (fila, columna) = fichas !! (seleccion - 1)
    putStrLn "Ingrese la nueva posición para la ficha."
    (nuevaFila, nuevaColumna) <- obtenerCoordenadas
    if esJugadaValida tablero nuevaFila nuevaColumna
        then do
            let nuevoTablero = actualizarTablero tablero fila columna ' '
            let nuevoTableroFinal = actualizarTablero nuevoTablero nuevaFila nuevaColumna (simbolo jugador)
            let nuevoFichas = actualizarFichas jugador seleccion (nuevaFila, nuevaColumna) fichasX fichasO
            imprimirTablero nuevoTableroFinal
            loop nuevoTableroFinal (cambiarJugador jugador) nuevoFichas
        else do
            putStrLn "Movimiento inválido. Intente de nuevo."
            moverFicha jugador tablero fichasX fichasO

-- Actualiza las coordenadas de una ficha después de moverla
actualizarFichas :: Jugador -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
actualizarFichas X seleccion nuevaFicha fichasX fichasO = take (seleccion - 1) fichasX ++ [nuevaFicha] ++ drop seleccion fichasX
actualizarFichas O seleccion nuevaFicha fichasX fichasO = take (seleccion - 1) fichasO ++ [nuevaFicha] ++ drop seleccion fichasO

-- Verifica si una jugada es válida (dentro de los límites y casilla vacía)
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
verificarLinea simbolo linea = linea == replicate 3 simbolo

-- Genera las dos diagonales posibles del tablero
diagonalesPrincipales :: Tablero -> [[Char]]
diagonalesPrincipales tablero =
    [[tablero !! i !! i | i <- [0..2]], [tablero !! i !! (2 - i) | i <- [0..2]]]

-- Obtiene una dimensión (número entero) de la entrada estándar
getDimensionInput :: IO Int
getDimensionInput = do
    input <- getLine
    if all isDigit input && (read input :: Int) >= 3
        then return (read input :: Int)
        else do
            putStrLn "Por favor ingrese un número válido (mayor o igual a 3)."
            getDimensionInput

-- Obtiene una coordenada (número entero) de la entrada estándar
getCoordinateInput :: IO Int
getCoordinateInput = do
    input <- getLine
    if all isDigit input && (read input :: Int) >= 1
        then return (read input :: Int)
        else do
            putStrLn "Por favor ingrese un número válido (mayor o igual a 1)."
            getCoordinateInput

-- Función para imprimir el tablero de manera visual
imprimirTablero :: Tablero -> IO ()
imprimirTablero tablero = do
    mapM_ (putStrLn . unwords . map return) tablero
    putStrLn ""  -- Añade una línea en blanco para separar
