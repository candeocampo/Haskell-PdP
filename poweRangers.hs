module Library where
import PdePreludat

-- punto 1 --
data Persona = Persona{
    habilidades :: [String],
    tipo :: Bool
}deriving(Show,Eq)

data PowerRanger = PowerRanger{
    color :: Color,
    habilidadesPower :: [String],
    nivel :: Number
}deriving(Show,Eq)
type Color = String

cande :: Persona
cande = Persona ["compu","bailar"] True 

billie :: PowerRanger
billie = PowerRanger "rosa" ["cantar","bailar"] 100

-- punto 2 -- 
convertirEnPoweRanger :: Color -> Persona -> PowerRanger
convertirEnPoweRanger colorDado persona = PowerRanger{
    color = colorDado,
    habilidadesPower = (potencialHabilidad . habilidades) persona,
    nivel = (sum . map length . habilidades) persona}

agregarSuper :: String -> String
agregarSuper hab = "super" ++ hab

potencialHabilidad' :: [String] -> [String]
potencialHabilidad' = map agregarSuper 

potencialHabilidad :: [String] -> [String]
potencialHabilidad = map ("super" ++)

-- punto 3 --
formarEquipoRanger :: [Color] -> [Persona] -> [PowerRanger]
formarEquipoRanger colores personas = zipWith convertirEnPoweRanger colores (buenasPersonas personas)

buenasPersonas :: [Persona] -> [Persona]
buenasPersonas = filter ((==True).tipo)

-- punto 4 -- 
findOrElse :: (a -> Bool) -> a -> [a] -> a
findOrElse condicion valor lista
    | any condicion lista = find condicion lista
    | otherwise = valor

find :: (a -> Bool) -> [a] -> a
find condicion = head . filter condicion

rangerLider :: [PowerRanger] -> PowerRanger
rangerLider powers = findOrElse ((=="rojo").color) (head powers) powers 

maximumBy :: Ord b => [a] -> (a -> b) -> a
maximumBy lista funcion = foldl (mayorSegun funcion) (head lista) lista

mayorSegun :: Ord b => (t -> b) -> t -> t -> t 
mayorSegun f a b
    | f a > f b = a
    | otherwise = b

rangerMasPoderoso :: [PowerRanger] -> PowerRanger
rangerMasPoderoso powers = maximumBy powers nivel

-- punto 6 -- 
rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso = (>5).length.habilidadesPower

-- punto 7 --
alfa5 :: PowerRanger
alfa5 = PowerRanger "metalico" ["reparar cosas", infiniteAy] 0

infiniteAy :: String
infiniteAy = concat (repeat "ay ") 




