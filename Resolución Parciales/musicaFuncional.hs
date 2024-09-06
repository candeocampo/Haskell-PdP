module Library where
import PdePreludat

-- Enunciado: 
-- https://www.utnianos.com.ar/foro/tema-parcial-pdep-parcial-funcional-2021-resuelto-profesor-n-scarcella

data Nota = Nota {
tono :: Number, -- Frecuencia medida en Hz
volumen :: Number, -- Volumen de reproducción medido en Db
duracion :: Number -- Tiempo de reproducción medido en segundos
} deriving (Eq, Show)

-- FUNCIONES AUXILIARES
cambiarVolumen :: (Number -> Number) -> Nota -> Nota
cambiarVolumen delta nota = nota { volumen = delta (volumen nota) }
-- Dada una función transformación y una nota retorna una copia de la
-- nota con el volumen igual al resultado de aplicar la transformación a
-- su volumen actual.

cambiarTono :: (Number -> Number) -> Nota -> Nota
cambiarTono delta nota = nota { tono = delta (tono nota) }
-- Dada una función transformación y una nota retorna una copia de la
-- nota con el tono igual al resultado de aplicar la transformación a
-- su tono actual.

cambiarDuracion :: (Number -> Number) -> Nota -> Nota
cambiarDuracion delta nota = nota { duracion = delta (duracion nota) }
-- Dada una función transformación y una nota retorna una copia de la
-- nota con la duración igual al resultado de aplicar la transformación a
-- su duración actual.

promedio :: [Number] -> Number
promedio lista = sum lista / length lista
-- Dada una lista de números retorna el valor promedio

-- punto 1 -- 
esAudible :: Nota -> Bool
esAudible nota = tonoNota nota && ((>10).volumen) nota

tonoNota :: Nota -> Bool
tonoNota nota = ((>20).tono) nota && ((<20000).tono) nota

esMolesta :: Nota -> Bool
esMolesta nota = esAudible nota && ((<250).tono) nota && ((>85).volumen) nota 
    || esAudible nota && ((>250).tono) nota && ((>55).volumen) nota

-- punto 2 --
type Cancion = [Nota]
silencioTotal :: Cancion -> Number
silencioTotal = sum . map duracion . filter (not.esAudible)

sinInterrupciones :: Cancion -> Bool
sinInterrupciones = all (esAudible) . filter ((>0.1).duracion)

peorMomento :: Cancion -> Number
peorMomento = maximum . map volumen . filter (esMolesta)

-- punto 3 --
type Filtro = Cancion -> Cancion

transponer :: Number -> Filtro
transponer escalar = map (cambiarTono (*escalar))

acotarVolumen :: Number -> Number -> Filtro 
acotarVolumen volMax volMin = map (cambiarVolumen (min volMax) . cambiarVolumen (max volMin))

-- Este filtro modifica cada nota de una canción seteando su
-- volumen al volumen promedio de la canción original.
normalizar :: Filtro
normalizar cancion = map (cambiarVolumen (const (volumenPromedio cancion))) cancion

volumenPromedio :: Cancion -> Number
volumenPromedio = promedio . map volumen

-- punto 4 --
-- a) Identifiar su tipo
-- f g [] y z = g y z
-- f g (x : xs) y z = g (x y) z || f g xs y z

-- Sería f :: (a -> b -> Bool) -> [a -> a] -> a -> b -> Bool

-- en termino de este parcial seria
f :: (Cancion -> Cancion -> Bool) -> [Filtro] -> Cancion -> Cancion -> Bool
f criterio filtro cancionO cancionS = criterio cancionO cancionS
f criterio (filtroX:filtroXS) cancionO cancionS = 
 criterio (filtroX cancionO) cancionS || f criterio filtroXS cancionO cancionS

-- b) 
infringeCopyright :: [Filtro] -> Cancion -> Cancion -> Bool
infringeCopyright = f (==)

-- c)
compararSegun :: (Cancion -> Cancion -> Bool) -> [Filtro] -> Cancion -> Cancion -> Bool
compararSegun criterio filtros cancionO cancionS = 
    criterio cancionO cancionS || produceMismaCancion filtros cancionO cancionS

produceMismaCancion :: [Filtro] -> Cancion -> Cancion -> Bool
produceMismaCancion filtros cancionO cancionS = cancionS == foldl aplicarFiltro cancionO filtros

aplicarFiltro :: Cancion -> Filtro -> Cancion
aplicarFiltro cancion filtro = filtro cancion

-- d) 
-- Definir una función tunear que dada una lista de Filtros y una Canción retorne la
-- Canción resultante de aplicarle todos los filtros a la canción original y, por último, la
-- normalice.

tunear :: Cancion -> [Filtro] -> Cancion
tunear cancion = normalizar . foldl aplicarFiltro cancion
