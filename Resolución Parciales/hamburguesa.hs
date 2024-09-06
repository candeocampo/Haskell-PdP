module Lib where
import PdePreludat

data Combo = Combo{
    hamburguesa :: [Ingrediente],
    bebida :: Bebida,
    acompaniamiento :: String
}deriving(Show,Eq)

type Ingrediente = String

data Bebida = Bebida{
    tipoBebida :: String,
    tamañoBebida :: Tamaño,
    light :: Bool
}deriving(Show,Eq)

type Tamaño = Number
regular = 1
mediano = 2 
grande = 3 

informacionNutricional :: [(String, Number)]
informacionNutricional = [("Carne",250), ("Queso",50), ("Pan",20), ("Panceta",541), ("Lechuga",5),("Tomate",6)]

tipoIngrediente = fst
caloriaNum = snd

condimentos = ["Barbacos","Mostaza","Mayonesa","Ketchup"]

-- punto 1 --
calorias :: Ingrediente -> Number
calorias ingrediente
    | ingrediente `elem` map fst informacionNutricional  = caloriaAlimento ingrediente informacionNutricional
    | otherwise = 10 

caloriaAlimento :: Ingrediente -> [(String, Number)] -> Number
caloriaAlimento ingrediente = tomarCaloria . tomarPrimerAlimento . filter (encontrarAlimento ingrediente)

tomarCaloria :: (String,Number) -> Number
tomarCaloria = snd 

tomarPrimerAlimento :: [(String, Number)] -> (String,Number)
tomarPrimerAlimento  = head 

encontrarAlimento :: Ingrediente -> (String,Number) -> Bool
encontrarAlimento ingrediente elemento = (== ingrediente) $ fst elemento

-- punto 2 -- 
esMortal :: Combo -> Bool
esMortal combo = (not.bebidaDietetica.bebida) combo && hamburguesaBomba combo

bebidaDietetica :: Bebida -> Bool
bebidaDietetica= (== True).light

hamburguesaBomba :: Combo -> Bool
hamburguesaBomba combo = algunoTieneMasCalorias combo ||  ((>1000) . totalIngredientes) combo

algunoTieneMasCalorias :: Combo -> Bool
algunoTieneMasCalorias = any (>300) . map calorias . hamburguesa

totalIngredientes :: Combo -> Number
totalIngredientes = sum . map calorias . hamburguesa

{-- 
hamburguesaBomba :: [(String, Number)] -> Bool
hamburguesaBomba informacionNutricional = ((>1000) . totalCaloriasHamburguesa) informacionNutricional || tieneAlgunoConMasDe300Calorias informacionNutricional

tieneAlgunoConMasDe300Calorias :: [(String, Number)] -> Bool
tieneAlgunoConMasDe300Calorias = any (>300) . map snd 

totalCaloriasHamburguesa :: [(String, Number)] -> Number
totalCaloriasHamburguesa = sum . map snd 
--}

-- punto 3 --
type Alteracion = Combo -> Combo

cambiarAcompañiamiento :: String -> Alteracion
cambiarAcompañiamiento nuevoAcompañiamiento combo = combo{acompaniamiento= acompaniamiento combo ++ "," ++ nuevoAcompañiamiento}

siguienteTamaño :: Tamaño -> Tamaño
siguienteTamaño t
    | t == regular = mediano
    | t == mediano = grande
    | otherwise = t  -- Si es grande, no hay siguiente tamaño

-- Función para cambiar el tamaño de la bebida al siguiente
cambiarTamañoSiguiente :: Bebida -> Bebida
cambiarTamañoSiguiente bebida = bebida { tamañoBebida = siguienteTamaño (tamañoBebida bebida) }

-- Función para agrandar la bebida en el combo
agrandarBebida :: Alteracion
agrandarBebida combo = combo { bebida = cambiarTamañoSiguiente (bebida combo) }

peroSin :: Restriccion -> Alteracion
peroSin restriccion combo = combo{hamburguesa = (filter (not.restriccion).hamburguesa) combo}

type Restriccion = Ingrediente -> Bool

esCondimento :: Restriccion
esCondimento ingrediente = ingrediente `elem` condimentos

masCaloricoQue :: Number -> Restriccion 
masCaloricoQue valor ingrediente =  valor < calorias ingrediente

realizarCambio :: Combo -> [Alteracion] -> Combo
realizarCambio combo alteraciones = foldl (flip($)) combo alteraciones

esQueso :: Restriccion
esQueso ingrediente = ingrediente == "Queso"

-- punto 4 --
comboDePrueba :: Combo
comboDePrueba = Combo ["Lechuga","Tomate","Carne","Queso"] coca []
coca :: Bebida
coca = Bebida "Coca cola" 1 False
{-- 
Si hiciera una consulta sería 
> realizarCambio comboDePrueba [agrandarBebida grande, cambiarAcompañamiento "Ensalada Cesar", peroSin esCondimento, peroSin (masCaloricoQue 400), peroSin esQueso]
--}

-- punto 5 --
alivianan :: [Alteracion] -> Combo -> Bool
alivianan alteraciones combo = esMortal combo && not(esMortal(realizarCambio combo alteraciones))









