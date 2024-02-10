data Inmueble = Inmueble
    { año :: Int,
      metros :: Int,
      habitaciones :: Int,
      garaje :: Bool,
      zona :: Char
    } deriving (Show)

calcularPrecio :: Inmueble -> Double
calcularPrecio inmueble
    | zona inmueble == 'A' = precioBase * (1 - fromIntegral (2024 - año inmueble) / 100)
    | zona inmueble == 'B' = precioBase * (1 - fromIntegral (2024 - año inmueble) / 100) * 1.5
    where
        precioBase = fromIntegral (metros inmueble * 1000 + habitaciones inmueble * 5000 + if garaje inmueble then 15000 else 0)

buscarInmuebles :: [Inmueble] -> Double -> [(Inmueble, Double)]
buscarInmuebles [] _ = []
buscarInmuebles (x:xs) presupuesto
    | precio <= presupuesto = (x, precio) : buscarInmuebles xs presupuesto
    | otherwise = buscarInmuebles xs presupuesto
    where
        precio = calcularPrecio x

-- Ejemplo de uso
inmuebles :: [Inmueble]
inmuebles =
    [ Inmueble { año = 2000, metros = 100, habitaciones = 3, garaje = True, zona = 'A' },
      Inmueble { año = 2012, metros = 60, habitaciones = 2, garaje = True, zona = 'B' },
      Inmueble { año = 1980, metros = 120, habitaciones = 4, garaje = False, zona = 'A' },
      Inmueble { año = 2005, metros = 75, habitaciones = 3, garaje = True, zona = 'B' },
      Inmueble { año = 2015, metros = 90, habitaciones = 2, garaje = False, zona = 'A' }
    ]

presupuesto :: Double
presupuesto = 100000.0

inmueblesFiltrados :: [(Inmueble, Double)]
inmueblesFiltrados = buscarInmuebles inmuebles presupuesto

-- Mostrar inmuebles filtrados
mostrarInmuebles :: [(Inmueble, Double)] -> IO ()
mostrarInmuebles [] = putStrLn "No se encontraron inmuebles dentro del presupuesto."
mostrarInmuebles inmuebles = mapM_ (print) inmuebles

main :: IO ()
main = mostrarInmuebles inmueblesFiltrados
