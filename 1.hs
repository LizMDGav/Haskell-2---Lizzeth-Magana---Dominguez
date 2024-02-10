import System.IO ( hFlush, stdout )
import Data.List (intercalate)
import Text.Printf

-- Diccionario de funciones matemáticas
funciones :: [(String, Double -> Double)]
funciones =
  [ ("sin", sin),
    ("cos", cos),
    ("tan", tan),
    ("exp", exp),
    ("log", log)
  ]

-- Solicitar función al usuario
solicitarFuncion :: IO String
solicitarFuncion = do
  putStr "Ingrese la función que desea aplicar (sin, cos, tan, exp, log): "
  hFlush stdout
  funcion <- getLine
  if funcion `elem` map fst funciones
    then return funcion
    else solicitarFuncion

-- Solicitar valor al usuario
solicitarValor :: IO Int
solicitarValor = do
  putStr "Ingrese un valor entero mayor que cero: "
  hFlush stdout
  valor <- readLn
  if valor > 0
    then return valor
    else solicitarValor

-- Calcular lista de resultados
calcularResultados :: String -> Int -> [Double]
calcularResultados funcion n = map (\i -> funciones' funcion (fromIntegral i)) [1 .. n]
  where
    funciones' f = case lookup f funciones of
      Just func -> func
      Nothing -> error "Función no encontrada"

-- Mostrar tabla
mostrarTabla :: String -> [Double] -> IO ()
mostrarTabla funcion resultados = do
  putStrLn $ "Función: " ++ funcion
  putStrLn $ "Valor | Resultado"
  putStrLn $ "------|----------"
  mapM_ (\(i, res) -> printf "%-5d | %.5f\n" (i :: Int) res) (zip [1 ..] resultados)

main :: IO ()
main = do
  funcion <- solicitarFuncion
  valor <- solicitarValor
  let resultados = calcularResultados funcion valor
  mostrarTabla funcion resultados
