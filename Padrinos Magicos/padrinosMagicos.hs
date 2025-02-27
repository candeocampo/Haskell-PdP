module Library where
import PdePreludat

-- parte A --

data Chico = Chico{
    nombre :: String,
    edad :: Number,
    habilidades :: [String],
    deseos :: [Deseo]
}deriving(Show,Eq)

type Deseo = Chico -> Chico

timmy :: Chico
timmy = Chico "Timmy" 10 ["mirar television","jugar en la pc"] [serMayor]

-- punto 1 -- 
aprenderHabilidades :: [String] -> Chico -> Chico
aprenderHabilidades habilidadesNuevas chico = chico{habilidades = habilidades chico ++ habilidadesNuevas}

serGrosoEnNeedForSpeed :: Chico -> Chico
serGrosoEnNeedForSpeed = aprenderHabilidades agregarNeedForSpeed 

agregarNeedForSpeed :: [String]
agregarNeedForSpeed = map generarHabilidadNeedForSpeed [1..]

generarHabilidadNeedForSpeed :: Number -> String
generarHabilidadNeedForSpeed n = "jugar need for speed " ++ show n

serMayor :: Chico -> Chico
serMayor chico = chico{edad=18}

-- punto 2 -- 
type Padrino = Chico -> Chico
wanda :: Padrino
wanda = modificarEdad (+1) . cumplirPrimerDeseo

modificarEdad :: (Number -> Number)  -> Chico -> Chico
modificarEdad transformacion chico = chico{edad=(transformacion.edad)chico}

cumplirPrimerDeseo :: Chico -> Chico 
cumplirPrimerDeseo chico = (head.deseos) chico chico

cosmo :: Padrino
cosmo = modificarEdad (`div`2)

muffinMagico :: Padrino
muffinMagico chico = foldl cumplirDeseos chico (deseos chico)

cumplirDeseos :: Chico -> Deseo -> Chico
cumplirDeseos chico deseo = deseo chico

-- parte B --
-- punto 1 --
type Condicion = Chico -> Bool
type Habilidad = String

tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad habilidad = elem habilidad .habilidades

esSuperMaduro :: Condicion
esSuperMaduro chico = ((>18).edad) chico && tieneHabilidad "manejar" chico

-- punto 2 --
data Chica = Chica{
    nombreChica :: String,
    condicionParaElegirChico :: Condicion
}deriving(Show,Eq)

trixie :: Chica
trixie = Chica "Trixie Tang" noEsTimmy

vicky :: Chica
vicky = Chica "Vicki" (tieneHabilidad "ser un supermodelo noruego")

noEsTimmy :: Condicion
noEsTimmy = (/= "Timmy").nombre

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA chica [pretendiente] = pretendiente
quienConquistaA chica pretendientes 
    | any (condicionParaElegirChico chica) pretendientes = (head . filter (condicionParaElegirChico chica)) pretendientes
    | otherwise = last pretendientes

-- parte c --
infractoresDeDaRules:: [Chico] -> [String]
infractoresDeDaRules  = map nombre . filter tieneDeseoProhibido

tieneDeseoProhibido :: Chico -> Bool
tieneDeseoProhibido chico = (any (tieneDeseoProhibidoPara chico) . deseos) chico

tieneDeseoProhibidoPara :: Chico -> Deseo -> Bool
tieneDeseoProhibidoPara deseo = tieneAlgunaHabilidadProhibida . cumplirDeseos deseo

tieneAlgunaHabilidadProhibida :: Chico -> Bool
tieneAlgunaHabilidadProhibida =  any esHabilidadProhibida . take 5 . habilidades

esHabilidadProhibida :: Habilidad -> Bool
esHabilidadProhibida unaHabilidad = elem unaHabilidad habilidadesProhibidas

habilidadesProhibidas :: [String] 
habilidadesProhibidas = ["enamorar","matar","dominar el mundo"]




