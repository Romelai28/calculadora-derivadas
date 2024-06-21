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
-- simplificar :: (Floating a, Eq a) => Funcion a -> Funcion a
