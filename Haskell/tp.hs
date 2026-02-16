import Data.Char (isDigit)
import Data.List (transpose)

-- Tipo de datos para representar el tablero y los jugadores
type Tablero = [[Char]]
data Jugador = X | O deriving (Show, Eq)

-- Función principal
main :: IO ()
main = do
    putStrLn "Bienvenido a Tres en Línea"
    putStrLn "Ingrese el número de filas (n >= 3):"
    n <- getIntInput
    putStrLn "Ingrese el número de columnas (m >= 3):"
    m <- getIntInput
    jugarNuevoJuego n m

-- Inicia un nuevo juego
jugarNuevoJuego :: Int -> Int -> IO ()
jugarNuevoJuego n m = do
    let tablero = replicate n (replicate m ' ')
    jugarTurno tablero X

-- Función para obtener una entrada de número entero mayor o igual a 3
getIntInput :: (Int -> Bool) -> IO Int
getIntInput validacion = do
    input <- getLine
    if esEntero input
        then let n = read input :: Int
             in if validacion n then return n else repetir
        else repetir
  where
    repetir = do
        putStrLn "Entrada inválida, intente nuevamente."
        getIntInput validacion


esEntero :: String -> Bool
esEntero xs = all isDigit xs

-- Muestra el tablero
-- Muestra el tablero
mostrarTablero :: Tablero -> IO ()
mostrarTablero tablero = do
    putStrLn $ "  " ++ unwords (map show [1 .. length (head tablero)])
    mapM_ putStrLn [show i ++ " " ++ unwords (map (\c -> "|" ++ [c]) fila) ++ "|" | (i, fila) <- zip [1..] tablero]

-- Alterna turnos entre jugadores
jugarTurno :: Tablero -> Jugador -> IO ()
jugarTurno tablero jugador = do
    mostrarTablero tablero
    putStrLn $ "Turno de " ++ show jugador ++ ". Ingrese fila y columna:"
    (fila, col) <- leerMovimiento
    if movimientoValido tablero fila col
        then do
            let nuevoTablero = actualizarTablero tablero fila col (simbolo jugador)
            if hayGanador nuevoTablero (simbolo jugador)
                then do
                    mostrarTablero nuevoTablero
                    putStrLn $ "¡Jugador " ++ show jugador ++ " gana!"
                    reiniciarJuego
                else
                    jugarTurno nuevoTablero (alternarJugador jugador)
        else do
            putStrLn "Movimiento inválido, intente nuevamente."
            jugarTurno tablero jugador

-- Lee y valida el movimiento del jugador
leerMovimiento :: IO (Int, Int)
leerMovimiento = do
    putStr "Fila: "
    fila <- getIntInput
    putStr "Columna: "
    col <- getIntInput
    return (fila - 1, col - 1)

-- Verifica si el movimiento es válido
movimientoValido :: Tablero -> Int -> Int -> Bool
movimientoValido tablero fila col =
    fila >= 0 && fila < length tablero &&
    col >= 0 && col < length (head tablero) &&
    tablero !! fila !! col == ' '

-- Actualiza el tablero con la jugada
actualizarTablero :: Tablero -> Int -> Int -> Char -> Tablero
actualizarTablero tablero fila col jugador =
    [if i == fila
        then [if j == col then jugador else tablero !! i !! j | j <- [0..(length (head tablero) - 1)]]
        else tablero !! i | i <- [0..(length tablero - 1)]]


-- Verifica si hay un ganador
hayGanador :: Tablero -> Char -> Bool
hayGanador tablero jugador =
    any (== True) [alineado fila | fila <- tablero] ||
    any (== True) [alineado col | col <- transpose tablero] ||
    alineado (diagonal tablero) || alineado (diagonal (map reverse tablero))
  where
    alineado :: [Char] -> Bool
    alineado xs = jugador `elem` xs && length (filter (== jugador) xs) >= 3

diagonal :: Tablero -> [Char]
diagonal tablero = [tablero !! i !! i | i <- [0 .. min (length tablero) (length (head tablero)) - 1]]

-- Alterna entre jugadores
alternarJugador :: Jugador -> Jugador
alternarJugador X = O
alternarJugador O = X

simbolo :: Jugador -> Char
simbolo X = 'X'
simbolo O = 'O'

-- Permite reiniciar el juego
reiniciarJuego :: IO ()
reiniciarJuego = do
    putStrLn "¿Quieres jugar de nuevo? (s/n)"
    respuesta <- getLine
    if respuesta == "s" then main else putStrLn "¡Gracias por jugar!"
