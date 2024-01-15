import System.IO

data Frac = Frac Int Int

instance Show Frac
    where
        show (Frac a b) =
            (show a) ++
            "/" ++
            (show b)


readFrac = do
    putStrLn "Ingresa numerador"
    num <- getLine
    putStrLn "Ingresa divisor"
    div <- getLine
    let res = Frac (read num) (read div)
    putStrLn ("Tu fracción es " ++ show res)


    
parteFrac (Frac a b) =
    Frac (a*2) (b*2)

