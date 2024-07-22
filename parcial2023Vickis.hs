module Library where
import PdePreludat 

data Investigador = Investigador{
    nombre :: String,
    cordura :: Number,
    items :: [Item],
    sucesosEvitados :: [String]
} deriving (Show, Eq)

data Item = Item{
    nombreItem :: String,
    valor :: Number
} deriving (Show, Eq)

maximoSegun :: Ord a => (b -> a) -> [b] -> b 
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (b -> a) -> b -> b -> b 
mayorSegun f a b
 |f a > f b = a
 |otherwise = b 

deltaSegun :: (a -> Number) -> (a -> a) -> a -> Number
deltaSegun ponderacion transformacion valor =
    abs ((ponderacion. transformacion) valor - ponderacion valor)

-- Punto 1 --
-- a --
investigadorEnloquece :: Number -> Investigador -> Investigador
investigadorEnloquece puntos investigador = investigador{
    cordura = max 0 (cordura investigador - puntos)
} 
-- b --
hallarItem :: Item -> Investigador -> Investigador
hallarItem item investigador = investigador{
    items = items investigador ++ [item]
}

-- Punto 2 --
tieneItemConNombre :: String -> [Investigador] -> Bool
tieneItemConNombre nombre = any (investigadorTieneItem nombre)

investigadorTieneItem :: String -> Investigador -> Bool
investigadorTieneItem nombre = elem nombre. listaNombresItem 

listaNombresItem :: Investigador -> [String]
listaNombresItem = map nombreItem. items 

-- Punto 3 --
determinarLider :: [Investigador] -> Investigador
determinarLider = maximoSegun potencial

potencial :: Investigador -> Number
potencial investigador
 |totalmenteLoco investigador = 0
 |otherwise = cordura investigador * (experiencia investigador + valorMaximoItem investigador)

experiencia :: Investigador -> Number
experiencia = (+) 1. (*) 3. length. sucesosEvitados

valorMaximoItem :: Investigador -> Number
valorMaximoItem = maximoSegun id. listaValorItem

listaValorItem :: Investigador -> [Number]
listaValorItem = map valor. items

totalmenteLoco :: Investigador -> Bool
totalmenteLoco = (==) 0. cordura

-- Punto 4 --
-- a --
deltaCorduraTotal :: Number -> [Investigador] -> Number
deltaCorduraTotal punto = 
    foldr ((+). deltaSegun cordura (investigadorEnloquece punto)) 0 

-- b --
deltaPotencial1Integrante :: [Investigador] -> Number
deltaPotencial1Integrante = 
    deltaSegun (potencial. head) perderInvestigadorLocos

perderInvestigadorLocos :: [Investigador] -> [Investigador]
perderInvestigadorLocos = filter (not. totalmenteLoco)

-- c --
investigadorInfinitos :: Investigador -> [Investigador]
investigadorInfinitos investigador = investigador : investigadorInfinitos investigador
{-
En el punto a no se va a poder obtener una respuesta independientemente de como la utilicemos
de forma ansiosa o perezosa va a intentar evaluar desde afuera hacia adentro y como 
la cantidad de investigadores es infinita no va a poder llegar a una respuesta(? 
En combio en el punto b al estar agarrando solo el primer elemento de la lista de 
investigadores infinitos nos va a dar un resultado, si el investigador no esta totalmente loco. (? x2
-}

-- Punto 5 --

type Suceso = [Investigador] -> [Investigador]
type Consecuencias = [[Investigador] -> [Investigador]]

moldeSucesos :: String -> Consecuencias -> ([Investigador] -> Bool) -> [Investigador] -> [Investigador]
moldeSucesos nombre consecuencias formaEvitar lista 
 |formaEvitar lista = map (agregarSuceso nombre) lista 
 |otherwise = realizarConsecuencias consecuencias lista 

agregarSuceso :: String -> Investigador -> Investigador
agregarSuceso suceso investigador = investigador{
    sucesosEvitados = sucesosEvitados investigador ++ [suceso]
} 

realizarConsecuencias :: Consecuencias -> [Investigador] -> [Investigador]
realizarConsecuencias listaConsecuencias lista = 
    foldl (flip ($)) lista listaConsecuencias

todosEnloquecen :: Number -> [Investigador] -> [Investigador]
todosEnloquecen puntos = map (investigadorEnloquece puntos)

agregarItem :: Item -> Investigador -> Investigador
agregarItem item investigador = investigador{
    items = items investigador ++ [item]
}

agregarItemAlPrimero :: Item -> [Investigador] -> [Investigador]
agregarItemAlPrimero item lista =  agregarItem item (head lista) : tail lista

despertarDeUnAntiguo :: Suceso
despertarDeUnAntiguo = 
    moldeSucesos "Despertar de un antiguo" [todosEnloquecen 10, tail] (tieneItemConNombre "Necronomicon") 

ritualEnInnsmouth :: Suceso
ritualEnInnsmouth =
    moldeSucesos "Ritual en Innsmouth" [agregarItemAlPrimero dagaMaldita, todosEnloquecen 2, despertarDeUnAntiguo] ((>) 100. potencial. determinarLider) 

dagaMaldita :: Item
dagaMaldita = Item "Daga Maldita" 3

-- Punto 6 --
enfrentarSuceso :: Suceso -> [Investigador] -> [Investigador]
enfrentarSuceso suceso = suceso. map (investigadorEnloquece 1)

-- Punto 7 --
masAterrador :: [Investigador] -> [Suceso] -> Suceso
masAterrador _[suceso] = suceso
masAterrador lista (suceso1:suceso2:suceso3) 
 |deltaSegun corduraTotal suceso1 lista > deltaSegun corduraTotal suceso2 lista  = masAterrador lista (suceso1:suceso3)
 |otherwise = masAterrador lista (suceso2:suceso3)

corduraTotal :: [Investigador] -> Number
corduraTotal = foldr ((+). cordura) 0 