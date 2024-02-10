filtro :: (a -> Bool) -> [a] -> [a]
filtro _ [] = []  -- Caso base: la lista vacía no contiene elementos que cumplan el filtro
filtro f (x:xs)
    | f x       = x : filtro f xs  -- Si el elemento cumple el filtro, lo agregamos a la lista resultante
    | otherwise = filtro f xs      -- Si no cumple, lo omitimos y continuamos con el resto de la lista

-- Ejemplo de uso:
-- Filtrar los números pares de una lista
ejemplo :: [Int]
ejemplo = filtro even [1, 2, 3, 4, 5, 6, 7, 8, 9]