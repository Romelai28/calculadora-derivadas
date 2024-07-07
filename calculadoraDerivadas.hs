-- Pendiente, considerar la posibilidad de pedir que los casos marcados como problematicos puedan pedir que el fijo reduzca a una constante.

data MathExp a = X
                | Cte a
                | Suma (MathExp a) (MathExp a)
                | Prod (MathExp a) (MathExp a)
                | Frac (MathExp a) (MathExp a)
                | LogNat (MathExp a)
                | LogBase a (MathExp a)             -- PROBLEMATICO
                | Exp_e (MathExp a)  -- e^a
                | Potencia_base_fija a (MathExp a)  -- PROBLEMATICO
                | Potencia (MathExp a) a            -- PROBLEMATICO
                | Sin (MathExp a)
                | Cos (MathExp a)
                | Tan (MathExp a)
                deriving (Eq,Show,Read)


instance Num a => Num (MathExp a) where
    (+) = Suma
    (*) = Prod
    negate = Prod (Cte (-1))
    a - b = a + (negate b)
    fromInteger = Cte . fromInteger


instance (Fractional a, Num a) => Fractional (MathExp a) where
    (/) = Frac
    fromRational = Cte . fromRational



factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*factorial (n-1)


foldFunc :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (a -> b -> b) -> (b -> b)-> (a -> b-> b) -> (b -> a -> b) -> (b -> b) -> (b -> b) -> (b -> b) -> MathExp a -> b
foldFunc cX cCte cSuma cProd cFrac cLogNat cLogBase cExp_e cPotencia_base_fija cPotencia cSin cCos cTan func = case func of
    X -> cX
    Cte k -> cCte k
    Suma f g -> cSuma (rec f) (rec g)
    Prod f g -> cProd (rec f) (rec g)
    Frac f g -> cFrac (rec f) (rec g)
    LogNat f -> cLogNat (rec f)
    LogBase base f -> cLogBase base (rec f)
    Exp_e f -> cExp_e (rec f)
    Potencia_base_fija base f -> cPotencia_base_fija base (rec f)
    Potencia f n -> cPotencia (rec f) n
    Sin f -> cSin (rec f)
    Cos f -> cCos (rec f)
    Tan f -> cTan (rec f)
  where rec = foldFunc cX cCte cSuma cProd cFrac cLogNat cLogBase cExp_e cPotencia_base_fija cPotencia cSin cCos cTan


evaluar :: (Floating a, Eq a) => a -> MathExp a -> a
evaluar arg = foldFunc arg id (+) (*) (/) (log) (logBase) (exp) (**) (**) (sin) (cos) (tan)


esConstante :: MathExp a -> Bool
esConstante (Cte _) = True
esConstante _       = False

-- Se pued escribir como un fold pero no era claro.
derivar :: (Floating a, Eq a) => MathExp a -> MathExp a
derivar func = case func of
    X            -> 1
    Cte _        -> 0
    Suma f g     -> (derivar f) + (derivar g)
    Prod f g     -> f * (derivar g) + (derivar f) * g
    Frac f g     -> ((derivar f) * g - f * (derivar g)) / (Potencia g 2)

    LogNat f     -> (derivar f) / f
    LogBase b f  -> (derivar f) / (f * (LogNat (Cte b)))

    Exp_e f      -> (Exp_e f) * (derivar f)
    Potencia_base_fija b f -> (Potencia_base_fija b f) * (LogNat (Cte b)) * (derivar f)
    Potencia f n -> Cte n * (Potencia f (evaluar 727 (Cte n - 1))) * (derivar f)

    Sin f        -> (Cos f) * (derivar f)
    Cos f        -> (negate (Sin f)) * (derivar f)
    Tan f        -> (1 + (Tan f)*(Tan f)) * (derivar f)


n_derivar :: (Floating a, Eq a) => Integer -> MathExp a -> MathExp a
n_derivar 0 f = f
n_derivar n f = n_derivar (n-1) (simplificar . derivar $ f)


taylor :: (Floating a, Eq a) => a -> Integer -> MathExp a -> MathExp a
taylor a n f = taylorAux a 0 n f (Cte 0)

taylorAux :: (Floating a, Eq a) => a -> Integer -> Integer -> MathExp a -> MathExp a -> MathExp a
taylorAux a iterador n f poli
    | iterador > n = poli
    | otherwise = taylorAux a (iterador+1) n (simplificar . derivar $ f)
                    (simplificar (poli +
                                    ((Cte (evaluar a f)) / (Cte (fromIntegral(factorial iterador)))) * (Potencia (X + (negate (Cte a))) (fromIntegral iterador))
                                 )
                    )


recta_tangente :: (Floating a, Eq a) => a -> MathExp a -> MathExp a
recta_tangente a f = m * (X - Cte a) + Cte (evaluar a f)
    where m = Cte (evaluar a (derivar f))
-- ejemplo: formato (recta_tangente 2 (6+X*X+5*X))



-- Un par de podas para simplificar las expresiones
-- Las cte's son izquiedistas.
simplificar :: (Floating a, Eq a) => MathExp a -> MathExp a
simplificar func = case func of
    X -> X
    Cte k -> Cte k

    Suma (Cte 0) f -> simplificar f     -- 0 neutro de la suma.
    Suma (Cte a) (Cte b) -> Cte (a+b)   -- Suma entre cte's.

    Suma f (Cte a) | not (esConstante f) -> simplificar (Suma (Cte a) f)                    -- localmente mando las cte a la izquierda.
    Suma f (Suma (Cte a) g) | not (esConstante f) -> simplificar (Suma (Cte a) (Suma f g))  -- doy un paso hacia afuera, mando las cte afuera a la izquierda.
    Suma (Cte a) (Suma (Cte b) f) -> Suma (Cte (a+b)) (simplificar f)                       -- Si tengo 2 cte a la izquierda, las opero entre sí.

    -- unificar sumas a productos (Problema, según su ubicación dentro del producto no unifican necesariamente)
    Suma f g | f == g -> simplificar (Prod (Cte 2) f)
    Suma f (Prod (Cte a) g) | f == g -> simplificar (Prod (Cte (a+1)) f)
    Suma (Prod (Cte a) f) g | f == g -> simplificar (Prod (Cte (a+1)) f)

    Suma f g ->
        let f_simp = simplificar f
            g_simp = simplificar g
        in if f == f_simp && g == g_simp
            then Suma f g
            else simplificar (Suma f_simp g_simp)
    
    Prod (Cte 0) f -> Cte 0             -- 0 absorbente del producto.
    Prod (Cte 1) f -> simplificar f     -- 1 neutro del producto
    Prod (Cte a) (Cte b) -> Cte (a*b)   -- Producto entre cte's.

    Prod f (Cte a) | not (esConstante f) -> simplificar (Prod (Cte a) f)                    -- localmente mando las cte a la izquierda.
    Prod f (Prod (Cte a) g) | not (esConstante f) -> simplificar (Prod (Cte a) (Prod f g))  -- doy un paso hacia afuera, mando las cte afuera a la izquierda.
    Prod (Cte a) (Prod (Cte b) f) -> Prod (Cte (a*b)) (simplificar f)                       -- Si tengo 2 cte a la izquierda, las opero entre sí.

    -- unificar productos a potencias (Problema, según su ubicación dentro del producto no unifican necesariamente)
    Prod f g | f == g -> simplificar (Potencia f 2)                   -- temporal?
    Prod f (Potencia g n) | f == g -> simplificar (Potencia f (n+1))  -- temporal?
    Prod (Potencia f n) g | f == g -> simplificar (Potencia f (n+1))  -- temporal?

    Prod (Potencia f n) (Potencia g m) | f == g -> simplificar (Potencia f (n+m))          -- Propiedad: Producto de potencias de misma base.
    Prod (Potencia_base_fija a f) (Potencia_base_fija b g) | a == b -> simplificar (Potencia_base_fija a (Prod f g))  -- Propiedad: Producto de potencias de misma base.

    
    Prod f g ->
        let f_simp = simplificar f
            g_simp = simplificar g
        in if f == f_simp && g == g_simp
            then Prod f g
            else simplificar (Prod f_simp g_simp)

    Frac f g | f == g -> Cte 1
    Frac f (Cte 1) -> simplificar f
    Frac (Cte 0) _ -> Cte 0  -- Si habilito experimental, esto es reduntante
    -- Experimental:  (No usar fracciones)
    Frac f g -> simplificar (Prod f (Potencia g (-1)))
    -- Si se habilita experimental, esto de abajo es reduntante.
    Frac f g ->
        let f_simp = simplificar f
            g_simp = simplificar g
        in if f == f_simp && g == g_simp
            then Frac f g
            else simplificar (Frac f_simp g_simp)

    LogNat (Cte 1) -> Cte 0
    LogNat (Exp_e (Cte 1)) -> Cte 1
    LogNat f ->
        let f_simp = simplificar f
        in if f == f_simp
            then LogNat f
            else simplificar (LogNat f_simp)

    LogBase _ (Cte 1) -> Cte 0
    LogBase a (Cte b) | a == b -> Cte 1
    LogBase b f ->
        let f_simp = simplificar f
        in if f == f_simp
            then LogBase b f
            else simplificar (LogBase b f_simp)
    
    Exp_e (Cte 0) -> Cte 1
    Exp_e f ->
        let f_simp = simplificar f
        in if f == f_simp
            then Exp_e f
            else simplificar (Exp_e f_simp)

    Potencia_base_fija _ (Cte 0) -> Cte 1  -- Considero 0^0 es 1
    Potencia_base_fija b (Cte 1) -> Cte b
    Potencia_base_fija b f ->
        let f_simp = simplificar f
        in if f == f_simp
            then Potencia_base_fija b f
            else simplificar (Potencia_base_fija b f_simp)

    Potencia f 1 -> simplificar f
    Potencia _ 0 -> Cte 1  -- Considero 0^0 es 1
    Potencia (Potencia f n) m -> simplificar (Potencia f (n*m))
    Potencia f n ->
        let f_simp = simplificar f
        in if f == f_simp
            then Potencia f n
            else simplificar (Potencia f_simp n)
    
    Sin f ->
        let f_simp = simplificar f
        in if f == f_simp
            then Sin f
            else simplificar (Sin f_simp)

    Cos f ->
        let f_simp = simplificar f
        in if f == f_simp
            then Cos f
            else simplificar (Cos f_simp)

    Tan f ->
        let f_simp = simplificar f
        in if f == f_simp
            then Tan f
            else simplificar (Tan f_simp)


-- Devuelve una string de la función más amigable de leer. (Genera parentesis redundates)
formatoNoSimp :: (Floating a, Eq a, Show a) => MathExp a -> String
formatoNoSimp func = case func of
    X -> "X"
    Cte k -> show k
    Suma f g -> "(" ++ formatoNoSimp f ++ "+" ++ formatoNoSimp g ++ ")"
    Prod (Cte (-1)) f -> "(-" ++ formatoNoSimp f ++ ")"
    Prod f g -> "(" ++ formatoNoSimp f ++ "*" ++ formatoNoSimp g ++ ")"
    Frac f g -> "(" ++ formatoNoSimp f ++ "/" ++ formatoNoSimp g ++ ")"
    LogNat f -> "ln(" ++ formatoNoSimp f ++ ")"
    LogBase b f -> "log_" ++ show b ++ "_(" ++ formatoNoSimp f ++ ")"
    Exp_e f -> "e^{" ++ formatoNoSimp f ++ "}"
    Potencia_base_fija b f -> show b ++ "^{" ++ formatoNoSimp f ++ "}"
    Potencia f n -> formatoNoSimp f ++ "^{" ++ show n ++ "}"  -- ¿Hace falta agregar parentesis a la f?
    Sin f -> "sin(" ++ formatoNoSimp f ++ ")"
    Cos f -> "cos(" ++ formatoNoSimp f ++ ")"
    Tan f -> "tan(" ++ formatoNoSimp f ++ ")"


formato :: (Floating a, Eq a, Show a) => MathExp a -> String
formato = formatoNoSimp . simplificar


-- La solución para eliminar las \ fue usando putStrLn, me quedo una función auxiliar (toLatexNoSimp) y otra principal (toLatex)
toLatexNoSimp :: (Floating a, Eq a, Show a) => MathExp a -> String
toLatexNoSimp func = case func of
    X -> "{x}"
    Cte k -> show k
    Suma f g -> "(" ++ toLatexNoSimp f ++ " + " ++ toLatexNoSimp g ++ ")"
    Prod (Cte (-1)) f -> "(-" ++ toLatexNoSimp f ++ ")"
    Prod f g -> "(" ++ toLatexNoSimp f ++ " \\cdot " ++ toLatexNoSimp g ++ ")"
    Frac f g -> "\\frac{" ++ toLatexNoSimp f ++ "}{" ++ toLatexNoSimp g ++ "}"
    LogNat f -> "\\ln{(" ++ toLatexNoSimp f ++ ")}"
    LogBase b f -> "\\log_" ++ show b ++ "{(" ++ toLatexNoSimp f ++ ")}"
    Exp_e f -> "{e}^{" ++ toLatexNoSimp f ++ "}"
    Potencia_base_fija b f -> "{" ++ show b ++ "}^{" ++ toLatexNoSimp f ++ "}"
    Potencia f n -> "{" ++ toLatexNoSimp f ++ "}^{" ++ show n ++ "}"  -- ¿Hace falta agregar parentesis a la f?
    Sin f -> "\\sin{(" ++ toLatexNoSimp f ++ ")}"
    Cos f -> "\\cos{(" ++ toLatexNoSimp f ++ ")}"
    Tan f -> "\\tan{(" ++ toLatexNoSimp f ++ ")}"


-- Pendiente agregar tipado de la función latex
toLatex = putStrLn . toLatexNoSimp . simplificar
