-- factorial n que calcula el factorial de n.

fact 0 = 1
fact n = n * fact(n-1)

potencia a 0 = 1
potencia a 1 = a
potencia a n =
    a * potencia a (n-1)

-- raices a b c que calcula las raices de ax + by + c = 0

raiz a b c = do 
    let x = potencia b 2 - (4 * a * c)
    if x<0 
        then (-0, -0)
        --Esto como error de no poder representar esta información con pares ordenados reales.
    else
        ((-b + sqrt x)/(2*a), (-b - sqrt x)/(2*a))
