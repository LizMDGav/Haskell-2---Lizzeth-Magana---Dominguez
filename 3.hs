calificacion :: Int -> String
calificacion nota
    | nota >= 95 && nota <= 100 = "Excelente"
    | nota >= 85 && nota <= 94 = "Notable"
    | nota >= 75 && nota <= 84 = "Bueno"
    | nota >= 70 && nota <= 74 = "Suficiente"
    | otherwise = "Desempenio insuficiente"

-- | Aplica la función 'calificacion' a cada nota en la lista.
calificaciones :: [Int] -> [String]
calificaciones notas = map calificacion notas