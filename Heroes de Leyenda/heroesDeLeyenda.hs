module Library where
import PdePreludat


data Heroe = Heroe{
    epiteto :: String, -- es el apodo
    reconocimiento :: Number,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}deriving(Show,Eq)

data Artefacto = Artefacto{
    nombre :: String,
    rareza :: Number
}deriving(Show,Eq)

-- Punto 2 --
paseALaHistoria :: Heroe -> Heroe
paseALaHistoria heroe 
    | ((>1000).reconocimiento) heroe = modificarEpiteto "El mitico" heroe
    | ((<=500).reconocimiento) heroe = (añadirArtefacto lanza . modificarEpiteto "El magnifico") heroe
    | between 100 500 (reconocimiento heroe) = (añadirArtefacto xiphos . modificarEpiteto "Hoplita") heroe
    | otherwise = heroe

lanza :: Artefacto
lanza = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

between n m x = elem x [n..m]

-- Punto 3 --
type Tarea = Heroe -> Heroe

modificarEpiteto :: String -> Tarea
modificarEpiteto nuevoEpiteto heroe = heroe{epiteto=nuevoEpiteto}

añadirArtefacto :: Artefacto -> Tarea
añadirArtefacto artefactoNuevo heroe = heroe{artefactos = artefactoNuevo : artefactos heroe}

aumentarReconomiento :: Number -> Tarea
aumentarReconomiento valor heroe = heroe{reconocimiento = reconocimiento heroe + valor}

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto artefacto = añadirArtefacto artefacto . aumentarReconomiento (rareza artefacto)

escalarElOlimpo :: Tarea
escalarElOlimpo = añadirArtefacto relampago . desecharArtefactos . triplicarRarezaArtefactos . aumentarReconomiento 500

relampago :: Artefacto
relampago = Artefacto "Relampago de Zeus" 500

triplicarRarezaArtefactos :: Heroe -> Heroe
triplicarRarezaArtefactos heroe = heroe{artefactos = map modificarRarezaArtefacto (artefactos heroe) }

modificarRarezaArtefacto :: Artefacto -> Artefacto
modificarRarezaArtefacto artefacto = artefacto{rareza= (*3).rareza $ artefacto} 

desecharArtefactos :: Heroe -> Heroe
desecharArtefactos heroe = heroe{artefactos= desechar (artefactos heroe)}

desechar :: [Artefacto] -> [Artefacto]
desechar = filter(not.(<1000).rareza)

ayudarACruzarLaCalle :: Number -> Tarea
ayudarACruzarLaCalle cuadras = epitetoInfinito "groso" cuadras

epitetoInfinito :: String -> Number -> Heroe -> Heroe
epitetoInfinito nombre cuadras heroe = heroe{epiteto="gros" ++ replicate cuadras 'o'}

data Bestia = Bestia{
    nombreBestia :: String,
    debilidad :: Heroe -> Bool
}deriving(Show,Eq)

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia bestia heroe 
    | debilidad bestia heroe = modificarEpiteto ("El asesino de" ++ nombreBestia bestia) heroe
    | otherwise = (perderPrimerArtefacto . modificarEpiteto "El cobarde" ) heroe

perderPrimerArtefacto :: Heroe -> Heroe
perderPrimerArtefacto heroe = heroe{artefactos=(tail.artefactos)heroe}

-- Punto 4 --
heracles :: Heroe
heracles = Heroe{
    epiteto = "Heracles",
    reconocimiento = 700,
    artefactos = [pistola, relampago],
    tareas = [matarAlLeonDeNemea]
}

pistola :: Artefacto
pistola = Artefacto "pistola" 1000

-- Punto 5 --
matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarUnaBestia leonDeNemea

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" epitetoHeroe

epitetoHeroe :: Heroe -> Bool
epitetoHeroe = (>=20).length.epiteto

-- Punto 6 --
hacerTarea :: Heroe -> Tarea -> Heroe
hacerTarea heroe tarea = tarea heroe

-- Punto 7 --                -- (ganador,perdedor)
presuman :: Heroe -> Heroe -> (Heroe,Heroe)
presuman heroe1 heroe2 
    | reconocimiento heroe1 > reconocimiento heroe2 = (heroe1,heroe2)
    | reconocimiento heroe1 < reconocimiento heroe2 = (heroe2,heroe1)
    | sumatoriaRarezas heroe1 > sumatoriaRarezas heroe2 = (heroe1,heroe2)
    | sumatoriaRarezas heroe1 < sumatoriaRarezas heroe2 = (heroe2,heroe1)
    | otherwise = presuman (realizarLabor heroe1 (tareas heroe2)) (realizarLabor heroe2 (tareas heroe1))

sumatoriaRarezas :: Heroe -> Number
sumatoriaRarezas = sum . map rareza . artefactos


-- Punto 9 -- 
realizarLabor :: Heroe -> [Tarea] -> Heroe
realizarLabor heroe = foldl hacerTarea heroe 

























