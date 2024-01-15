-- Le decimos al compilador nuestro nuevo tipo de dato "Frac"
data Frac = Frac Int Int

-- Le decimos al compilador cómo imprimir nuestro nuevo tipo de dato
instance Show Frac
    where
        show (Frac a b) =
            show a ++
            "/" ++
            show b

-- Definimos funciones (operaciones) con nuestro tipo de dato
sumaF (Frac a b) (Frac c d) =
    Frac (a*d + (c*b)) (b*d)

restaF (Frac a b) (Frac c d) =
    Frac (a*d - (c*b)) (b*d)

multF (Frac a b) (Frac c d) =
    Frac (a*c) (b*d)

divF (Frac a b) (Frac c d) =
    Frac (a*d) (b*c)