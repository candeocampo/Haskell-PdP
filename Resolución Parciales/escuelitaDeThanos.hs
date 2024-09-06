module Lib where
import PdePreludat

data Personaje = UnPersonaje{
    edad :: Number,
    energia :: Number,
    habilidades :: [Habilidad],
    nombre :: String,
    planetaVive :: String
}deriving(Show,Eq)

type Habilidad = String

data Guantelete = UnGuantelete{
    material :: String,
    gemas :: [Gema]
}deriving (Show,Eq)

type Gema = Personaje -> Personaje

type Universo = [Personaje]

-- PRIMERA PARTE

-- PUNTO 1 

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete = poseeMaterial guantelete && ((==6).length.gemas) guantelete

poseeMaterial :: Guantelete -> Bool 
poseeMaterial guantelete = material guantelete == "uru"

chasquearDedos :: Guantelete -> Universo -> Universo
chasquearDedos guantelete universo 
    | guanteleteCompleto guantelete = reducirUniverso universo 
    | otherwise = universo

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (length universo `div`2) universo

-- PUNTO 2
universoAptoParaPendex :: Universo -> Bool
universoAptoParaPendex = any ((<45).edad) 

energiaTotalDeUniverso :: Universo -> Number
energiaTotalDeUniverso = sum . map energia . filter ((>1).length.habilidades)

--SEGUNDA PARTE

--PUNTO 3

modificarEnergia :: (Number -> Number) -> Personaje -> Personaje
modificarEnergia transformacion personaje = UnPersonaje{energia=(transformacion.energia) personaje}

mente :: Number -> Gema
mente valorEnergia = modificarEnergia (flip (-) valorEnergia)

alma :: Habilidad ->  Gema
alma nuevaHabilidad = modificarEnergia (flip (-) 10).eliminarHabilidad nuevaHabilidad

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad nombreHabilidad personaje = UnPersonaje{habilidades=filter (/=nombreHabilidad) (habilidades personaje)}

poseeHabilidad :: Habilidad -> Personaje -> Bool
poseeHabilidad nombreHabilidad  = elem nombreHabilidad. habilidades 

espacio :: String -> Gema 
espacio nuevoPlaneta = transportarPlaneta nuevoPlaneta . modificarEnergia (flip (-) 20)

transportarPlaneta :: String -> Personaje -> Personaje 
transportarPlaneta nuevoPlaneta personaje = UnPersonaje{planetaVive=nuevoPlaneta}

poder :: Gema
poder = modificarEnergia (const 0)  .habilidadesPersonaje 

habilidadesPersonaje :: Personaje -> Personaje
habilidadesPersonaje personaje 
    | length(habilidades personaje) <= 2 = UnPersonaje{habilidades=[]}
    | otherwise = personaje

tiempo :: Gema
tiempo personaje 
    | edad personaje > 18 = (modificarEnergia (flip (-) 50) . modificarEdad) personaje
    | otherwise = UnPersonaje{edad=18}

modificarEdad :: Personaje -> Personaje
modificarEdad personaje = UnPersonaje{edad=edad personaje `div` 2}

gemaLoca :: Gema -> Gema 
gemaLoca gema = gema.gema

-- PUNTO 4
{-
Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” 
y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.
-}

guanteleteGoma :: Guantelete
guanteleteGoma = UnGuantelete{
    material = "goma",
    gemas = [tiempo,alma "usar Mjnolnir",gemaLoca (alma "programacion en Haskell")]
}

-- PUNTO 5
utilizar :: Personaje -> [Gema] -> Personaje
utilizar personaje gemas = foldl aplicarGema personaje gemas 

aplicarGema :: Personaje -> Gema -> Personaje
aplicarGema personaje gema = gema personaje

--PUNTO 6
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa  guantelete personaje = gemaDeMayorPoder personaje (gemas guantelete)

gemaDeMayorPoder :: Personaje -> [Gema] -> Gema
gemaDeMayorPoder _ [gema] = gema
gemaDeMayorPoder personaje (gema1:gema2:gemas) 
    | (energia . flip aplicarGema gema1) personaje > (energia. flip aplicarGema gema2) personaje= gemaDeMayorPoder personaje (gema1:gemas)
    | otherwise = gemaDeMayorPoder personaje (gema2:gemas)

--PUNTO 7
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas tiempo)

-- usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
-- usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

{-
Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
1) gemaMasPoderosa punisher guanteleteDeLocos
Esta función no se puede ejecutar debido a la evaluación perezoza con la que trabaja haskell, al intentar encontrar la gemaDeMayorPoder
en la lista infinita que tiene guanteleteDeLocos Haskell evaluaría toda la lista infinita haciendo que nunca termine de devolvernos un resultado. 
además de que esta teniendo un problema con el Show de Personaje

No converge un valor porque Haskell evalua una lista infinita en la recursividad de gemaDeMayorPoder 
ya que no puede mostrar la instancia show de la lista infinita.



2) usoLasTresPrimerasGemas guanteleteDeLocos punisher
Esta funcion se va a concetrar en evaluar las 3 primeras gemas de la lista por la función take, y aplicarlas
en la funcion "utilizar", de esta manera Haskell no sigue iterando de forma infinitamente.

Converge porque el take le permite a Haskell que evalue los 3 primeros elementos de la lista infinita.


-}




















