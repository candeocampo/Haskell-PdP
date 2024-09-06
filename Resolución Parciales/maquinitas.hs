module Lib where
import PdePreludat

data Persona = Persona{
    nombre :: String,
    dinero :: Number,
    suerte :: Number,
    factores :: [Factor]
}deriving(Show,Eq)

data Factor = Factor{
    nombreFactor :: String,
    valor :: Number,
    permitido :: Bool
}deriving(Show,Eq)

-- PUNTO 1

cosmeFulanito :: Persona
cosmeFulanito = Persona "Cosme fulanito "100 30 [amuleto,manosMagicas,paciencia (-10)]

elCoco :: Persona
elCoco = Persona "Coco" 20 50 [inteligencia, paciencia 55]

amuleto :: Factor
amuleto = Factor{
    nombreFactor = "Amuleto",
    valor = 3,
    permitido = True
}

manosMagicas :: Factor 
manosMagicas = Factor{
    nombreFactor = "Manos magicas",
    valor = 100,
    permitido = False
}

inteligencia :: Factor 
inteligencia = Factor{
    nombreFactor = "Inteligencia",
    valor = 55,
    permitido = True
}

paciencia :: Number -> Factor
paciencia valorNuevo= Factor{
    nombreFactor = "Paciencia",
    valor = valorNuevo,
    permitido = True
}

-- PUNTO 2
--1.a) Cuánto tiene para un determinado factor, retornando 0 si no lo tiene.
tienePara :: String -> Persona -> Number
tienePara factorBuscar persona
    | tieneFactor factorBuscar persona = valor (buscarFactor factorBuscar persona)
    | otherwise = 0

tieneFactor :: String -> Persona -> Bool 
tieneFactor factorBuscar = any ((== factorBuscar).nombreFactor).factores

buscarFactor :: String -> Persona -> Factor 
buscarFactor factorBuscar  = head.filter ((==factorBuscar).nombreFactor).factores

--1.b) El potencial de una persona, que es su suerte más la sumatoria 
-- del valor de sus factores dividida por 100.
potencial :: Persona -> Number
potencial persona = suerte persona + div ((sum.map valor.factores) persona) 100 

--1.c) Si es tramposa, que será cierto si alguno de sus factores
-- no está permitido en el casino, lo cual de ahora en más se indicará para cada facto
tramposa :: Persona -> Bool 
tramposa = any(not.estaPermitido).factores

estaPermitido :: Factor -> Bool 
estaPermitido factor = (==) True (permitido factor)

tramposa' :: Persona -> Bool 
tramposa' = any(not.permitido).factores

-- PUNTO 3
-- a) Modelar a los juegos sabiendo que un juego se compone por un nombre, 
-- una forma de determinar cuánto dinero da como ganancia ante una apuesta 
-- y una serie de criterios determinantes para poder ganarlo.

data Juego = Juego{
    nombreJuego :: String,
    criterioGanar :: [Persona -> Bool],
    gananciaJuego :: Dinero -> Dinero
}deriving(Show,Eq)

type Dinero = Number

ruleta :: Juego
ruleta = Juego "Ruleta" [criterioRuleta] gananciaRuleta

criterioRuleta :: Persona -> Bool
criterioRuleta persona = not(tramposa persona) && (>70) (potencial persona)

gananciaRuleta :: Dinero -> Dinero
gananciaRuleta dinero = dinero * 37

-- Declarar la maquinita que se basa en un jackpot, 
-- y lo que se gana es ese jackpot + la apuesta. 
-- Para ganar se debe cumplir que la persona tenga una suerte mayor a 50 
-- y que tenga más de 40 de paciencia

maquinita :: Dinero -> Juego 
maquinita jackpot = Juego "Maquinita" [criterioMaquinita] (jackpot +)

criterioMaquinita :: Persona -> Bool 
criterioMaquinita persona = (>50) (suerte persona) && (>40) (tienePara "paciencia" persona)

blackJack :: Juego 
blackJack = Juego "BlackJack" [criterioBlackJack] (*2)

criterioBlackJack :: Persona -> Bool 
criterioBlackJack persona = (>30) (suerte persona) && not(tramposa persona)

-- Saber si un jugador puede ganar un juego, lo cual sucede si cumple
-- todas las condiciones para ganar ese juego.

puedeGanar :: Persona -> Juego -> Bool 
puedeGanar persona juego = all (cumpleCriterio persona) (criterioGanar juego)

cumpleCriterio :: Persona -> (Persona -> Bool) -> Bool 
cumpleCriterio persona criterio = criterio persona

-- cumpleCriterio' :: Persona -> Juego -> Bool 
-- cumpleCriterio' persona juego = (criterioGanar juego) persona
-- no se puede aplicar una lista de funciones directamente a un valor

-- PUNTO 4
-- Hacer que un jugador apueste en un juego, que implica que la persona baje su saldo
-- en el valor de la apuesta y luego juegue al juego. 
-- Al jugar, si puede ganar en ese juego, la persona debería aumentar su saldo 
-- en lo que gana en el juego, de lo contrario la persona no gana nada.

apostarJuego :: Dinero -> Juego -> Persona -> Persona
apostarJuego apuesta juego = jugarJuego apuesta juego . bajarSaldo apuesta 

jugarJuego :: Dinero -> Juego -> Persona -> Persona
jugarJuego apuesta juego persona 
    | puedeGanar persona juego = sumarSaldo apuesta persona
    | otherwise = persona

bajarSaldo :: Dinero -> Persona -> Persona
bajarSaldo apuesta persona 
    | apuesta < dinero persona = sumarSaldo  (-apuesta) persona
    | otherwise = persona

sumarSaldo :: Dinero -> Persona -> Persona
sumarSaldo apuesta persona = persona{dinero=dinero persona - apuesta}

-- PUNTO 5
-- Dado un jugador, una apuesta inicial y una lista de juegos, 
-- obtener la cantidad total de dinero que puede conseguir esa persona 
-- con ese monto si apuesta en cada juego lo conseguido en el juego anterior, 
-- evitando los juegos en los cuales no pueda ganar. 
-- Si no puede ganar en ningún juego, el resultado sería la apuesta inicial, 
-- ya que no apostaría en ninguno

obtenerDinero :: Dinero -> Persona -> [Juego] -> Dinero 
obtenerDinero apuesta persona juegos = (foldl (flip gananciaJuego) apuesta . filter (puedeGanar persona)) juegos























