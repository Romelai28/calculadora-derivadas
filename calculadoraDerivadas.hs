-- Pendiente, considerar la posibilidad de pedir que los casos marcados como problematicos puedan pedir que el fijo reduzca a una constante.

data Funcion a = X
                | Cte a
                | Suma (Funcion a) (Funcion a)
                | Prod (Funcion a) (Funcion a)
                | Frac (Funcion a) (Funcion a)
                | LogNat (Funcion a)
                | LogBase a (Funcion a)             -- PROBLEMATICO
                | Exp_e (Funcion a)  -- e^a
                | Potencia_base_fija a (Funcion a)  -- PROBLEMATICO
                | Potencia (Funcion a) a            -- PROBLEMATICO
                | Sin (Funcion a)
                | Cos (Funcion a)
                | Tan (Funcion a)
                deriving (Eq,Show,Read)


negativo :: (Floating a, Eq a) => Funcion a -> Funcion a
negativo = Prod (Cte (-1))


foldFunc :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (a -> b -> b) -> (b -> b)-> (a -> b-> b) -> (b -> a -> b) -> (b -> b) -> (b -> b) -> (b -> b) -> Funcion a -> b
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


evaluar :: (Floating a, Eq a) => a -> Funcion a -> a
evaluar arg = foldFunc arg id (+) (*) (/) (log) (logBase) (exp) (**) (**) (sin) (cos) (tan)


esConstante :: Funcion a -> Bool
esConstante (Cte _) = True
esConstante _       = False

-- Se pued escribir como un fold pero no era claro.
derivar :: (Floating a, Eq a) => Funcion a -> Funcion a
derivar func = case func of
    X            -> Cte 1
    Cte _        -> Cte 0
    Suma f g     -> Suma (derivar f) (derivar g)
    Prod f g     -> Suma (Prod f (derivar g)) (Prod (derivar f) g)
    Frac f g     -> Frac (Suma (Prod (derivar f) g) (negativo(Prod f (derivar g))))
                         (Potencia g 2)

    LogNat f     -> Frac (derivar f) (f)
    LogBase b f  -> Frac (derivar f)
                         (Prod f (LogNat (Cte b)))

    Exp_e f      -> Prod (Exp_e f) (derivar f)
    Potencia_base_fija b f -> Prod (Prod (Potencia_base_fija b f) (LogNat (Cte b))) (derivar f)
    Potencia f n -> Prod (Prod (Cte n) (Potencia f (evaluar 727 (Suma (Cte n) (negativo (Cte 1)))))) (derivar f)

    Sin f        -> Prod (Cos f) (derivar f)
    Cos f        -> Prod (negativo (Sin f)) (derivar f)
    Tan f        -> Prod (Suma (Cte 1) (Prod (Tan f) (Tan f))) (derivar f)


n_derivar :: (Floating a, Eq a) => Integer -> Funcion a -> Funcion a
n_derivar 0 f = f
n_derivar n f = n_derivar (n-1) (simplificar (derivar f))


-- Un par de podas para simplificar las expresiones
-- Las cte's son izquiedistas.
simplificar :: (Floating a, Eq a) => Funcion a -> Funcion a
simplificar func = case func of
    X -> X
    Cte k -> Cte k


    Suma (Cte 0) f -> simplificar f     -- 0 neutro de la suma.
    Suma (Cte a) (Cte b) -> Cte (a+b)   -- Suma entre cte's.

    Suma f (Cte a) | not (esConstante f) -> simplificar (Suma (Cte a) f)                    -- localmente mando las cte a la izquierda.
    Suma f (Suma (Cte a) g) | not (esConstante f) -> simplificar (Suma (Cte a) (Suma f g))  -- doy un paso hacia afuera, mando las cte afuera a la izquierda.
    Suma (Cte a) (Suma (Cte b) f) -> Suma (Cte (a+b)) (simplificar f)                       -- Si tengo 2 cte a la izquierda, las opero entre sí.

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
    Potencia _ 0 -> Cte 0
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
formato :: (Show a) => Funcion a -> String
formato func = case func of
    X -> "X"
    Cte k -> show k
    Suma f g -> "(" ++ formato f ++ "+" ++ formato g ++ ")"
    Prod f g -> "(" ++ formato f ++ "*" ++ formato g ++ ")"
    Frac f g -> "(" ++ formato f ++ "/" ++ formato g ++ ")"
    LogNat f -> "ln(" ++ formato f ++ ")"
    LogBase b f -> "log_" ++ show b ++ "_(" ++ formato f ++ ")"
    Exp_e f -> "e^{" ++ formato f ++ "}"
    Potencia_base_fija b f -> show b ++ "^{" ++ formato f ++ "}"
    Potencia f n -> formato f ++ "^{" ++ show n ++ "}"  -- ¿Hace falta agregar parentesis a la f?
    Sin f -> "sin(" ++ formato f ++ ")"
    Cos f -> "cos(" ++ formato f ++ ")"
    Tan f -> "tan(" ++ formato f ++ ")"


-- La solución para eliminar las \ fue usando putStrLn, me quedo una función auxiliar (toLatex) y otra principal (latex)
toLatex :: (Show a) => Funcion a -> String
toLatex func = case func of
    X -> "{x}"
    Cte k -> show k
    Suma f g -> "(" ++ toLatex f ++ " + " ++ toLatex g ++ ")"
    Prod f g -> "(" ++ toLatex f ++ " \\cdot " ++ toLatex g ++ ")"
    Frac f g -> "\\frac{" ++ toLatex f ++ "}{" ++ toLatex g ++ "}"
    LogNat f -> "\\ln{(" ++ toLatex f ++ ")}"
    LogBase b f -> "\\log_" ++ show b ++ "{(" ++ toLatex f ++ ")}"
    Exp_e f -> "{e}^{" ++ toLatex f ++ "}"
    Potencia_base_fija b f -> "{" ++ show b ++ "}^{" ++ toLatex f ++ "}"
    Potencia f n -> "{" ++ toLatex f ++ "}^{" ++ show n ++ "}"  -- ¿Hace falta agregar parentesis a la f?
    Sin f -> "\\sin{(" ++ toLatex f ++ ")}"
    Cos f -> "\\cos{(" ++ toLatex f ++ ")}"
    Tan f -> "\\tan{(" ++ toLatex f ++ ")}"


-- Pendiente agregar tipado de la función latex
latex = putStrLn . toLatex . simplificar
