module Lib where
import PdePreludat

data Investigador = Investigador{
    nombre :: String,
    cordura :: Number,
    items :: [Item],
    sucesosEvitados :: [String]
} deriving(Show,Eq)

data Item = Item{
    nombreItem :: String,
    valor :: Number
} deriving (Show,Eq)

-- PUNTO 1
modificarCordura :: (Number -> Number) -> Investigador -> Investigador
modificarCordura transformacion investigador = investigador{cordura= (transformacion.cordura)investigador}

enloqueza :: Number -> Investigador -> Investigador
enloqueza puntos = modificarCordura (max 0 . flip (-) puntos)

hallarItem :: Item -> Investigador -> Investigador
hallarItem itemBuscar  = enloqueza (valor itemBuscar) . incorporarItem itemBuscar

incorporarItem :: Item -> Investigador -> Investigador
incorporarItem itemNuevo investigador = investigador{items=itemNuevo:items investigador}

-- PUNTO 2
algunoTieneItem :: String -> [Investigador] -> Bool 
algunoTieneItem itemBuscar = any (buscarItem itemBuscar)

buscarItem :: String -> Investigador -> Bool
buscarItem itemBuscar  = elem itemBuscar . map nombreItem . items

-- PUNTO 3
-- funciones auxiliares
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b 
    | f a > f b = a
    | otherwise = b

lider :: [Investigador] -> Investigador
lider  = maximoSegun potencial 

potencial :: Investigador -> Number
potencial investigador 
    | not(locoInvestigador investigador) = cordura investigador * experienciaInvestigador investigador + itemsMaximo investigador
    | otherwise = 0

experienciaInvestigador :: Investigador -> Number 
experienciaInvestigador  = (+1) . (*3) . length . sucesosEvitados

itemsMaximo :: Investigador -> Number
itemsMaximo  = maximum . map valor .items

locoInvestigador :: Investigador -> Bool
locoInvestigador = (== 0).cordura

-- PUNTO 4
-- funcion auxiliar
deltaSegun ponderacion transformacion valor = 
    abs ((ponderacion.transformacion) valor - ponderacion valor )

--a)
deltaCordura :: Number -> [Investigador] -> Number 
deltaCordura valor  =  sum . map (deltaSegun cordura (enloqueza valor)) 

--b)
deltaPotencial :: [Investigador] -> Number 
deltaPotencial = deltaSegun (potencial . head) investigoresNoLocos

investigoresNoLocos :: [Investigador] -> [Investigador]
investigoresNoLocos = filter (not.locoInvestigador)

-- Conocer el delta en potencial del primer integrante si perdieran a todos los integrantes que se 
-- haya vuelto totalmente locos.

-- PUNTO 5

data Suceso = Suceso{
    descripcion :: String,
    evitarSuceso :: [Investigador] -> Bool,
    consecuenciasSuceso :: [[Investigador] -> [Investigador]] 
} deriving(Show,Eq)

noHaceNada = id

despertarAntiguo :: Suceso
despertarAntiguo = Suceso{
    descripcion = "Despertar de un antiguo",
    evitarSuceso = algunoTieneItem "Necronomicon",
    consecuenciasSuceso = [enloquecerInvestigadores 10,sacarPrimerIntegrante]
}

sacarPrimerIntegrante :: [Investigador] -> [Investigador]
sacarPrimerIntegrante = tail

enloquecerInvestigadores :: Number -> [Investigador] -> [Investigador]
enloquecerInvestigadores puntos = map (enloqueza puntos)

ritual :: Suceso
ritual = Suceso "Ritual en Innsmouth" liderConMayorPotencial [primerIntegrante dagaMaldita, enloquecerInvestigadores 2,enfrentarSuceso despertarAntiguo]

liderConMayorPotencial :: [Investigador] -> Bool 
liderConMayorPotencial = (>100). potencial . lider 

dagaMaldita :: Item
dagaMaldita = Item "Daga maldita" 3 

primerIntegrante :: Item -> [Investigador] -> [Investigador]
primerIntegrante item investigadores = hallarItem dagaMaldita (head investigadores) : tail investigadores 

-- PUNTO 6
-- Hacer una función para que un grupo de investigadores enfrenten un suceso.
-- Dado que todos los sucesos tienen un impacto traumático sobre las personas que lo enfrentan,
-- totos los investigadores del grupo primero enloquecen en 1 punto, independientemente de lo que ocurra
-- a continuación.
-- Luego, en caso de que cumplan con lo necesario para evitar que el suceso ocurra, lo esperado
-- es que todos incorporen la descripción del suceso evitado a su lista de sucesos evitados.
-- En caso de que no pueadn evitar el suceso, el grupo de investigadores deberá sufrir todas las consecuencias
-- que ese suceso prodduzca, una tras otra.

enfrentarSuceso :: Suceso -> [Investigador] -> [Investigador]
enfrentarSuceso suceso  = cumpleNecesarioParaEvitarSuceso suceso . enloquecerInvestigadores 1

cumpleNecesarioParaEvitarSuceso :: Suceso -> [Investigador] -> [Investigador]
cumpleNecesarioParaEvitarSuceso suceso investigadores  
    | evitarSuceso suceso investigadores = map (agregarSuceso (descripcion suceso)) investigadores
    | otherwise = sufrirConsecuencias suceso investigadores

sufrirConsecuencias :: Suceso -> [Investigador] -> [Investigador]
sufrirConsecuencias suceso investigadores = foldl (aplicarConsecuencias) investigadores (consecuenciasSuceso suceso)

aplicarConsecuencias :: [Investigador] -> ([Investigador]-> [Investigador]) -> [Investigador]
aplicarConsecuencias investigadores consecuencias = consecuencias investigadores

agregarSuceso :: String -> Investigador -> Investigador
agregarSuceso sucesoNuevo investigador = investigador{sucesosEvitados= sucesoNuevo :sucesosEvitados investigador}

-- PUNTO 7
-- Dada una lista de sucesos, determinar cuál sería el más aterrador para un grupo de investigadores,
-- que es aquel que maximice el delta en la cordura total del grupo luego de enfrentarse al suceso
-- en cuestión.

elMasAterrador :: [Suceso] -> [Investigador] -> Suceso 
elMasAterrador [suceso] _ = suceso 
elMasAterrador (suceso1:suceso2:sucesos) investigadores 
    | deltaCorduraTotal (enfrentarSuceso suceso1 investigadores) 
    > deltaCorduraTotal (enfrentarSuceso suceso2 investigadores) = elMasAterrador (suceso1:sucesos) investigadores
    | otherwise = elMasAterrador (suceso2:sucesos) investigadores

deltaCorduraTotal :: [Investigador] -> Number 
deltaCorduraTotal  = sum . map cordura

{-
deltaCordura :: Number -> [Investigador] -> Number 
deltaCordura valor  =  sum . map (deltaSegun cordura (enloqueza valor)) 

deltaSegun ponderacion transformacion valor = 
    abs ((ponderacion.transformacion) valor - ponderacion valor )
-}





