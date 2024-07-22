module Lib where
import PdePreludat


data Guantelete = Guantelete{
    material :: String,
    gemas :: [Gema]
}deriving(Show,Eq)

data Personaje = Personaje{
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    nombre :: String,
    planetaVive :: String
}deriving(Show,Eq)

type Gema = Personaje -> Personaje
type Universo = [Personaje]

-- PRIMERA PARTE -- 

chasquiarDedos ::  Guantelete -> Universo -> Universo 
chasquiarDedos guantelete universo 
    | guanteleteCompleto guantelete = reducirUniverso universo
    | otherwise = universo

guanteleteCompleto :: Guantelete -> Bool 
guanteleteCompleto guantelete =  ((<=6).length.gemas) guantelete && ((== "uru") . material) guantelete

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (length universo `div` 2) universo

-- PUNTO 2 -- 
-- Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran 
-- tienen menos de 45 años.

universoAptoParaPendex :: Universo -> Bool 
universoAptoParaPendex = any ((<45).edad) 

-- Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes 
-- que tienen más de una habilidad.

energiaTotalUniverso :: Universo -> Number 
energiaTotalUniverso  = sum . map energia .filter ((>1).length.habilidades)

-- SEGUNDA PARTE --

-- PUNTO 3
modificarEnergia :: (Number -> Number) -> Personaje -> Personaje 
modificarEnergia transformacion personaje = personaje{energia=(transformacion.energia)personaje}

mente :: Number -> Gema 
mente valor = modificarEnergia (subtract valor)

alma :: String -> Gema   
alma habilidadSacar  = modificarEnergia (subtract 10) . quitarHabilidad habilidadSacar

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad habilidadBuscar personaje = personaje{habilidades= filter (/= habilidadBuscar) (habilidades personaje)}

espacio :: String -> Gema 
espacio planetaNuevo = transportarPlaneta planetaNuevo . modificarEnergia (subtract 20)

transportarPlaneta :: String -> Personaje -> Personaje 
transportarPlaneta planet personaje = personaje{planetaVive=planet}

poder :: Gema 
poder personaje  = (modificarEnergia (subtract (energia personaje)) . modificarHabilidades) personaje

modificarHabilidades :: Personaje -> Personaje
modificarHabilidades personaje 
    | length (habilidades personaje) >= 2 = sacarHabilidades personaje
    | otherwise = personaje

sacarHabilidades :: Personaje -> Personaje 
sacarHabilidades personaje = personaje{habilidades=[]}

tiempo :: Gema 
tiempo = modificarEnergia (flip (-) 25) . restarEdad

restarEdad :: Personaje -> Personaje
restarEdad personaje 
    | edad personaje > 18 = personaje{edad=edad personaje `div` 2}
    | otherwise = personaje{edad=18}

gemaLoca :: Gema -> Gema 
gemaLoca gema = gema.gema

-- PUNTO 4
--  Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” 
-- y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete{
    material = "goma",
    gemas = [alma "usar mjolnir",gemaLoca (alma "programacion en haskell")]
}

-- PUNTO 5
-- Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta el poder de 
-- cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” 
-- sobre la víctima.

utilizar :: [Gema] -> Personaje -> Personaje 
utilizar gemas personaje = foldl aplicarGema personaje gemas

aplicarGema :: Personaje -> Gema -> Personaje 
aplicarGema personaje gema = gema personaje

-- PUNTO 6
-- Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del 
-- infinito que produce la pérdida más grande de energía sobre la víctima. 

gemaMasPoderosa :: Guantelete -> Personaje -> Gema 
gemaMasPoderosa guantelete personaje = obtenerGema personaje (gemas guantelete)

obtenerGema :: Personaje -> [Gema] -> Gema
obtenerGema _ [gema] = gema
obtenerGema personaje (gema1:gema2:gemas)
    | energia (gema1 personaje) > energia (gema2 personaje) = obtenerGema personaje (gema1:gemas)
    | otherwise = obtenerGema personaje (gema2:gemas)

-- PUNTO 7
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema : (infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- Justifique si se puede ejecutar (punisher seria un personaje)
-- A) gemaMasPoderosa punisher guanteleDeLocos
-- Esta funcion no va a funcionar ya que guanteleteDeLocos tiene una lista infinita de gemas por lo que la funcion
-- de gemaMasPoderosa no va a terminar nunca de evaluar la lista completa, esto no se puede realizar debido a la 
-- evaluacion perezosa de haskell.

--No converge un valor porque Haskell evalua una lista infinita en la recursividad de gemaDeMayorPoder 
--ya que no puede mostrar la instancia show de la lista infinita.

-- B) usoLasTresPrimerasGemas guanteleteDeLocos punisher
-- Sí se puede utilizar ya que para utilizar la funcion utilizar se usa las primeras 3 gemas del guantelete evitando recorrer
-- toda la lista infinita, de esta manera se evaluan los primeros tres elementos de la lista de gemas de guantelete. 
-- Converge porque el take le permite a Haskell que evalue los 3 primeros elementos de la lista infinita

