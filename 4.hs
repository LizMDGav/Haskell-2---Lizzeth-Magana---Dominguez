import qualified Data.Map as Map
import Data.Char (toUpper)

-- Definición de las notas y sus calificaciones correspondientes
data Calificacion = Excelente | Notable | Bueno | Suficiente | DesempeñoInsuficiente
    deriving (Show)

-- Función para asignar calificaciones según las notas
calificar :: Double -> Calificacion
calificar nota
    | nota >= 95 = Excelente
    | nota >= 85 = Notable
    | nota >= 75 = Bueno
    | nota >= 70 = Suficiente
    | otherwise = DesempeñoInsuficiente

-- Función principal que toma un diccionario de asignaturas y notas,
-- y devuelve otro diccionario con las asignaturas en mayúsculas y las calificaciones aprobadas
calificarAsignaturas :: Map.Map String Double -> Map.Map String Calificacion
calificarAsignaturas notas =
    Map.fromList [(map toUpper asignatura, calificar nota) | (asignatura, nota) <- Map.toList notas]

-- Ejemplo de uso
main :: IO ()
main = do
    let notasAlumno = Map.fromList [("Matemáticas", 92), ("Historia", 78), ("Inglés", 65)]
        calificaciones = calificarAsignaturas notasAlumno
    print calificaciones