module Lib where
import PdePreludat

data Ladron = Ladron{
    nombre :: String,
    habilidades :: [Habilidad],
    armas :: [Arma]
}deriving(Show,Eq)

data Rehen = Rehen{
    nombreRehen :: String,
    nivelComplot :: Number,
    nivelMiedo :: Number,
    plan :: [Plan]
}deriving(Show,Eq)

type Habilidad = String
type Arma = Rehen -> Rehen

-- Armas
deducirComplot :: (Number -> Number) -> Rehen -> Rehen
deducirComplot transformacion rehen = rehen{nivelComplot=(transformacion.nivelComplot) rehen}

aumentarMiedo :: (Number -> Number) -> Rehen -> Rehen
aumentarMiedo transformacion rehen = rehen{nivelMiedo=(transformacion.nivelMiedo) rehen}

pistola :: Number -> Arma
pistola calibre rehen = aumentarMiedo (+(3*length(nombreRehen rehen))) . deducirComplot (subtract(5*calibre)) $ rehen

ametralladora :: Number -> Arma
ametralladora balas rehen = aumentarMiedo (+balas) . deducirComplot (subtract(nivelComplot rehen `div`2)) $ rehen

-- Formas de intimidar a los rehenes
type Metodo = Ladron -> Rehen -> Rehen

hacerseElMalo :: Metodo
hacerseElMalo ladron rehen 
    | ((=="berlin").nombre) ladron = aumentarMiedo (+(length.habilidades)ladron) rehen
    | ((=="rio").nombre) ladron = deducirComplot (+ 20) rehen
    | otherwise = aumentarMiedo (+ 10) rehen

disparos :: Metodo
disparos ladron rehen = usarArma rehen . masMiedo rehen . armas $ ladron

usarArma :: Rehen -> Arma -> Rehen
usarArma rehen arma = arma rehen

masMiedo :: Rehen -> [Arma] -> Arma
masMiedo rehen [arma] = arma
masMiedo rehen (a1:a2:armas)
    | (nivelMiedo.usarArma rehen) a1 > (nivelMiedo.usarArma rehen) a2 = masMiedo rehen (a1:armas)
    | otherwise = masMiedo rehen (a2:armas)

-- Rehenes se revelan
puedeRebelarse :: Rehen -> Bool
puedeRebelarse rehen =  nivelComplot rehen > nivelMiedo rehen

type Plan = Ladron -> Ladron

atacarAlLadron :: Rehen -> Plan
atacarAlLadron compañero ladron = ladron{armas=drop ((length.nombreRehen) compañero) (armas ladron)}

esconderse :: Plan
esconderse ladron = ladron{armas=drop ((length.habilidades)ladron) (armas ladron)}

-- Punto 1 --
tokio :: Ladron
tokio = Ladron "Toki" ["trabajo psicologico", "entrar en moto"] [pistola 9, pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "Profesor" ["disfrazarse de linyera","disfrazarse de payaso","estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "Pablo" 40 30 [esconderse]

arturito :: Rehen
arturito = Rehen "Arturito" 70 50 [esconderse, atacarAlLadron pablo]

-- Punto 2 --
esInteligente :: Ladron -> Bool
esInteligente = (>=2).length.habilidades

-- Punto 3 --
agregarArma :: Arma -> Ladron -> Ladron
agregarArma nuevaArma ladron = ladron{armas= nuevaArma : armas ladron}

-- Punto 4 --
intimide :: Ladron -> Metodo -> Rehen -> Rehen
intimide ladron metodo rehen = metodo ladron rehen

-- Punto 5 --
calmeLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmeLasAguas ladron = filter calmados . map (intimide ladron disparos) 

calmados :: Rehen -> Bool
calmados = (>60).nivelComplot

-- Punto 6 --
puedeEscaparse :: Ladron -> Bool
puedeEscaparse = any (=="disfrazarse de") . habilidades

-- Punto 7 --
pintaMal :: [Ladron] -> [Rehen] -> Bool
pintaMal ladrones rehenes = promedioRehenes rehenes > promedioMiedo rehenes * (length.map armas) ladrones

promedioRehenes :: [Rehen] -> Number
promedioRehenes rehenes =  complototal rehenes / length rehenes

complototal :: [Rehen] -> Number
complototal = sum . map nivelComplot

promedioMiedo :: [Rehen] -> Number
promedioMiedo rehenes = miedoTotal rehenes / length rehenes

miedoTotal :: [Rehen] -> Number
miedoTotal = sum . map nivelMiedo

-- Punto 8 --
seRebelen :: Ladron -> [Rehen] -> Ladron
seRebelen ladron = foldl (flip($)) ladron . concatMap plan . antesDeRebelarse
                                           -- (concat map plan) . 

antesDeRebelarse :: [Rehen] -> [Rehen]
antesDeRebelarse = map (deducirComplot (subtract 10)) . filter puedeRebelarse

-- Punto 9 --
planValencia :: [Rehen] -> [Ladron] -> Number
planValencia rehenes = (*1000000)  . cantidadArmas . map (flip seRebelen rehenes) . cararAmetralladora 

cantidadArmas :: [Ladron] -> Number
cantidadArmas = sum . map (length . armas)

cararAmetralladora :: [Ladron] -> [Ladron]
cararAmetralladora = map (agregarArma (ametralladora 45))

-- Punto 12 --
funcion :: (a -> (b -> Bool)) -> (b -> [c]) -> a -> Number -> [b] -> Bool
funcion cond num lista str = (>str) . sum . map (length.num) . filter (lista cond)

 



