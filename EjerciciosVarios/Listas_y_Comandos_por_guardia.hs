-- Devuelve la primera mitad
firstHalf :: [a] -> [a]
firstHalf xs
    -- Sólo por protección
    | null xs = []
    | length xs == 1 = xs
    -- Lo relevante
    | odd (length xs) = take (length xs `div` 2) xs
    | otherwise = take ((length xs + 1) `div` 2) xs

-- Devuelve la segunda mitad
secondHalf :: [a] -> [a]
secondHalf xs
    -- Sólo por protección
    | null xs = []
    | length xs == 1 = xs
    -- Lo relevante
    | odd (length xs) = drop (length xs `div` 2) xs
    | otherwise = drop ((length xs + 1) `div` 2) xs

-- Utiliza las 2 funciones anteriores para particionar cadenas
halve xs
    --Por protección
    | null xs = [[],[]]
    -- Relevante
    | length xs == 1 = [[], xs]
    | otherwise = [firstHalf xs, secondHalf xs]

-- Ordenamiento
merge :: Ord a => [a] -> [a] -> [a]
-- Por protección
merge xs [] = xs
merge [] ys = ys
-- Relevante
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Algoritmo principal
msort :: Ord a => [a] -> [a]
msort xs
    | null xs = []
    | length xs == 1 = xs
    | otherwise = do{
        merge (msort (head (halve xs))) (msort (last (halve xs)))
    }