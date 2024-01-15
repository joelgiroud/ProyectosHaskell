-- PROGRAMA QUE RESUELVE EL ALGORITMO NEWTON-RAPHSON
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Prelude hiding (words)
import qualified Data.Char as Char

-------------------------------
-- Definición de f
f ::  Num a => a -> a
f x = (x^3)-7

-- Definición de f' = g
g :: Num a => a -> a
g x = 3*(x^2)

-- Función auxiliar
iterateF :: (Num a, Fractional a) => a -> a
iterateF a =
      a - (f a)/(g a)

-- Función que nos servirá para comparar iteraciones (redondea)
truncate' :: (RealFrac a, Integral p) => a -> p -> a
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

-- Función principal
newtonR :: RealFrac t => t -> t
newtonR x0 = 
    if truncate' (iterateF x0) 3 == truncate' x0 3 then
        x0
    else
        newtonR (iterateF x0)

------------------------------------------------------

-- Función para quitar espacios
words   :: String -> [String]
words s =  case dropWhile Char.isSpace s of
        -- Definimos casos para nuestra frase
        "" -> []
        s' -> w : words s''
            where (w, s'') = break Char.isSpace s'
