module Lib where
import PdePreludat

type Cancion = [Nota]

data Nota = Nota {
tono :: Number, -- Frecuencia medida en Hz
volumen :: Number, -- Volumen de reproducción medido en Db
duracion :: Number -- Tiempo de reproducción medido en segundos
} deriving (Eq, Show)

-- FUNCIONES AUXILIARES
cambiarVolumen :: (Number -> Number) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la
-- nota con el volumen igual al resultado de aplicar la transformación a
-- su volumen actual.
cambiarVolumen delta nota = nota { volumen = delta (volumen nota) }

cambiarTono :: (Number -> Number) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la
-- nota con el tono igual al resultado de aplicar la transformación a
-- su tono actual.
cambiarTono delta nota = nota { tono = delta (tono nota) }

cambiarDuracion :: (Number -> Number) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la
-- nota con la duración igual al resultado de aplicar la transformación a
-- su duración actual.
cambiarDuracion delta nota = nota { duracion = delta (duracion nota) }

promedio :: [Number] -> Number
-- Dada una lista de números retorna el valor promedio
promedio lista = sum lista / length lista

-- Punto 1 --
esAudible :: Nota -> Bool
esAudible nota = between 20 20000 (tono nota) && ((>10).volumen) nota

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between a b num = elem num [a.. b]

esMolesta :: Nota -> Bool
esMolesta nota = esAudible nota && ((<250).tono) nota && ((>85).volumen) nota 
    || esAudible nota && ((>=250).tono) nota && ((>55).volumen) nota

-- Punto 2 --
silencioTotal :: Cancion -> Number
silencioTotal = sum . map duracion . filter (not.esAudible)

sinInterrupciones :: Cancion -> Bool
sinInterrupciones = all esAudible. filter tieneDuracionMayor

tieneDuracionMayor :: Nota -> Bool
tieneDuracionMayor = (>0.1).duracion

peorMomento :: Cancion -> Number
peorMomento = maximum . map volumen . filter esMolesta

-- Punto 3 --
type Filtro = Cancion -> Cancion

transponer :: Number -> Filtro
transponer escalar = map (cambiarTono (*escalar))

acotarVolumen :: Number -> Number -> Filtro
acotarVolumen volMax volMin = map (cambiarDuracion (min volMax) . cambiarVolumen (max volMin))

normalizar :: Filtro
normalizar cancion =  map (cambiarVolumen (const (volumenPromedio cancion))) cancion

volumenPromedio :: Cancion -> Number
volumenPromedio = promedio . map volumen

-- Punto 4 --
f g [] y z = g y z
f g (x : xs) y z = g (x y) z || f g xs y z

-- El más generico seria f :: (a -> b -> Bool) -> [a -> a] -> a -> b -> Bool
-- En contexto del parcial f :: (Cancion -> Cancion -> Bool) -> [Filtro] -> Cancion -> Cancion -> Bool

infringeCopyright :: [Filtro] -> Cancion -> Cancion -> Bool
infringeCopyright  = f (==)


compararSegun :: (Cancion -> Cancion -> Bool) -> [Filtro] -> Cancion -> Cancion -> Bool
compararSegun criterio [] cancion1 cancion2 = criterio cancion1 cancion2
compararSegun criterio (f1:fs) cancion1 cancion2 = criterio (f1 cancion1) cancion2 || any (\filtro -> criterio (filtro cancion1) cancion2) fs

-- Punto 5 --
tunear :: Cancion -> [Filtro] -> Cancion
tunear cancion = normalizar .  foldl (flip($)) cancion 



