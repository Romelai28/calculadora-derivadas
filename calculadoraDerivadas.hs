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


negativo :: (Floating a, Eq a) => Funcion a -> Funcion a
negativo = Prod (Cte (-1))


-- Se pued escribir como un fold pero no era claro.
derivar :: (Floating a, Eq a) => Funcion a -> Funcion a
derivar func = case func of
    X            -> Cte 1
    Cte _        -> Cte 0
    Suma f g     -> Suma (derivar f) (derivar g)
    Prod f g     -> Suma (Prod f (derivar g)) (Prod (derivar f) g)
    Frac f g     -> Frac (Suma (Prod (derivar f) g) (negativo(Prod f (derivar g))))
                        (Prod g g)
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
n_derivar n f = n_derivar (n-1) (derivar f)


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


-- Pendiente: simplificar.
simplificar :: (Floating a, Eq a) => Funcion a -> Funcion a
simplificar func = case func of
    X -> X
    Cte k -> Cte k

    Suma (Cte 0) f -> simplificar f
    Suma f (Cte 0) -> simplificar f
    Suma f g ->
        let f_simp = simplificar f
            g_simp = simplificar g
        in if f == f_simp && g == g_simp
            then Suma f g
            else simplificar (Suma f_simp g_simp)  -- Necesita el simplificar de verdad aca?
    
    Prod (Cte 0) f -> Cte 0
    Prod f (Cte 0) -> Cte 0
    Prod (Cte 1) f -> simplificar f
    Prod f (Cte 1) f -> simplificar f
    Prod f g ->
        let f_simp = simplificar f
            g_simp = simplificar g
        in if f == f_simp && g == g_simp
            then Suma f g
            else simplificar (Prod f_simp g_simp)  -- Necesita el simplificar de verdad aca?

    Frac f f -> Cte 1
    Frac f (Cte 1) -> simplificar f
    Frac f g ->
        let f_simp = simplificar f
            g_simp = simplificar g
        in if f == f_simp && g == g_simp
            then Suma f g
            else simplificar (Frac f_simp g_simp)  -- Necesita el simplificar de verdad aca?

    LogNat (Cte 1) -> Cte 0
    LogNat (Exp_e (Cte 1)) -> Cte 1
    LogNat f ->
        let f_simp = simplificar f
        in if f == f_simp
            then LogNat f
            else simplificar (LogNat f_simp)  -- Necesita el simplificar de verdad aca?

    LogBase _ 1 -> Cte 0
    LogBase b b -> Cte 1
    LogBase b f ->
        let f_simp = simplificar f
        in if f == f_simp
            then LogBase b f
            else simplificar (LogBase b f_simp)  -- Necesita el simplificar de verdad aca?
    
    Exp_e (Cte 1) -> Cte 0
    Exp_e f =
        let f_simp = simplificar f
        in if f == f_simp
            then Exp_e f
            else simplificar (Exp_e f_simp)  -- Necesita el simplificar de verdad aca?

    













    

simplificar (Prod _ (Cte 0)) = Cte 0
simplificar (Prod (Cte 0) _) = Cte 0
simplificar f = f

-- Ver de eliminar una de las \.
toLatex :: (Show a) => Funcion a -> String
toLatex func = case func of
    X -> "{x}"
    Cte k -> show k
    Suma f g -> "(" ++ toLatex f ++ "+" ++ toLatex g ++ ")"
    Prod f g -> "(" ++ toLatex f ++ "\\cdot" ++ toLatex g ++ ")"
    Frac f g -> "\\frac{" ++ toLatex f ++ "}{" ++ toLatex g ++ "}"
    LogNat f -> "\\ln{(" ++ toLatex f ++ ")}"
    LogBase b f -> "\\log_" ++ show b ++ "{(" ++ toLatex f ++ ")}"
    Exp_e f -> "e^{" ++ toLatex f ++ "}"
    Potencia_base_fija b f -> show b ++ "^{" ++ toLatex f ++ "}"
    Potencia f n -> toLatex f ++ "^{" ++ show n ++ "}"  -- ¿Hace falta agregar parentesis a la f?
    Sin f -> "\\sin{(" ++ toLatex f ++ ")}"
    Cos f -> "\\cos{(" ++ toLatex f ++ ")}"
    Tan f -> "\\tan{(" ++ toLatex f ++ ")}"
