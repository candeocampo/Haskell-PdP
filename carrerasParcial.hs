module Lib where
import PdePreludat

data Auto = Auto{
    color :: String,
    velocidad :: Number,
    distanciaRecorrida :: Number
}deriving(Show,Eq)

type Carrera = [Auto]

-- PUNTO 1 --

--1A)
-- Saber si un auto está cerca de otro auto, que se cumple si son autos distintos 
-- y la distancia que hay entre ellos (en valor absoluto) es menor a 10.
estaCerca :: Auto -> Auto -> Bool 
estaCerca auto1 auto2 = auto1 /= auto2 && distanciaAutos auto1 auto2 < 10

distanciaAutos :: Auto -> Auto -> Number 
distanciaAutos auto1  = abs . (distanciaRecorrida auto1 -) . distanciaRecorrida

--1B)
-- Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos 
-- (por haber recorrido más distancia que los otros)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo autoCorre carrera = not(noTieneAutoCerca autoCorre carrera) && (lesVaGanando autoCorre carrera)

noTieneAutoCerca :: Auto -> Carrera -> Bool 
noTieneAutoCerca auto = any (/=auto)  

lesVaGanando :: Auto -> Carrera -> Bool 
lesVaGanando autoCorre = all (vaGanando autoCorre) . filter (/=autoCorre) 

vaGanando :: Auto -> Auto -> Bool 
vaGanando autoCorre auto2 = distanciaRecorrida autoCorre > distanciaRecorrida auto2

--1C)
-- Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando.
puesto :: Auto -> Carrera -> Number 
puesto auto = (+1). length . filter (not.vaGanando auto)

-- PUNTO 2 --
type EstadoAuto = Auto -> Auto

corra :: Number -> EstadoAuto
corra tiempo auto = auto{distanciaRecorrida=distanciaRecorrida auto + tiempo * velocidad auto}

--2B.i)
-- A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que su 
-- velocidad final sea la resultante de usar dicho modificador con su velocidad actual.

alterarVelocidad :: (Number -> Number) -> Auto -> Auto 
alterarVelocidad transformacion auto = auto{velocidad=(transformacion.velocidad)auto}

--2B.ii)
-- Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se 
-- le reste a la velocidad actual la cantidad indicada, y como mínimo quede en 0, 
-- ya que no es válido que un auto quede con velocidad negativa.

bajarVelocidad :: Number -> EstadoAuto
bajarVelocidad num = alterarVelocidad (max 0 . subtract num )

-- PUNTO 3 
--funcion auxiliar
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

--3A)
type PowerUp = Auto -> Carrera -> Carrera
 
terremoto :: PowerUp
terremoto autoQueGatilla = afectarALosQueCumplen (estaCerca autoQueGatilla) (bajarVelocidad 50)

-- Miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad 
-- los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló 
-- el power up les vaya ganando.

miguelitos :: Number -> PowerUp
miguelitos velocidadCambiar autoQueGatilla = afectarALosQueCumplen (vaGanando autoQueGatilla) (bajarVelocidad velocidadCambiar)

-- jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. 
-- El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
-- Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante 
-- el tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
-- Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo

jetPack :: Number -> PowerUp
jetPack tiempo autoGatilla = 
    afectarALosQueCumplen (==autoGatilla) (alterarVelocidad (/2). corra tiempo . alterarVelocidad (*2)) 

-- PUNTO 4 -- 

-- que permita obtener la tabla de posiciones a partir del estado final de la carrera, 
-- el cual se obtiene produciendo cada evento uno detrás del otro, partiendo del estado de la carrera recibido.

type Evento  =  Carrera -> Carrera
type Color = String

simularCarrera :: Carrera -> [Evento] -> [(Number, Color)]
simularCarrera carrera eventos = (tablaPosiciones . producirEvento eventos) carrera

tablaPosiciones :: Carrera -> [(Number, Color)]
tablaPosiciones carrera = map (aplicarATabla carrera) carrera

aplicarATabla :: Carrera -> Auto -> (Number, Color)
aplicarATabla carrera auto = (puesto auto carrera, color auto)

producirEvento :: [Evento] -> Carrera -> Carrera 
producirEvento eventos carrera = foldl aplicarEvento carrera eventos

aplicarEvento :: Carrera -> Evento -> Carrera 
aplicarEvento carrera evento = evento carrera

--4B.i)
-- correnTodos que hace que todos los autos que están participando de la carrera corran durante un tiempo indicado.

correnTodos :: Number -> Evento
correnTodos tiempo autos = map (corra tiempo) autos

--ii)
-- usaPowerUp que a partir de un power up y del color del auto que gatilló el poder en cuestión, 
-- encuentre el auto correspondiente dentro del estado actual de la carrera para usarlo y produzca 
-- los efectos esperados para ese power up.

usaPowerUp :: PowerUp -> Color -> Evento 
usaPowerUp powerup colorAuto carrera = powerup (encontrarAuto colorAuto carrera) carrera

encontrarAuto :: Color -> Carrera -> Auto 
encontrarAuto colorBuscar  carrera = head (filter (buscarAuto colorBuscar) carrera)

buscarAuto :: Color -> Auto -> Bool
buscarAuto colorBuscar auto2 = colorBuscar == color auto2

--4C)
-- V1 
autosDeEjemplo :: [Auto]
autosDeEjemplo = map (\color -> Auto color 120 0) ["rojo", "blanco", "azul", "negro"]

--Variante 2
autoRojo :: Auto
autoRojo = Auto "rojo" 120 0

autoBlanco :: Auto 
autoBlanco = Auto "blanco" 120 0

autoAzul :: Auto
autoAzul = Auto "azul" 120 0

autoNegro :: Auto
autoNegro = Auto "negro" 120 0

carreraAutos :: Carrera
carreraAutos = [autoRojo,autoBlanco,autoAzul,autoNegro]

ejemploDeSimularCarrera = simularCarrera carreraAutos [
    correnTodos 30,
    usaPowerUp (jetPack 3) "azul",
    usaPowerUp terremoto "blanco",
    correnTodos 40,
    usaPowerUp (miguelitos 20) "blanco",
    usaPowerUp (jetPack 6) "negro",
    correnTodos 10 
    ]

-- PUNTO 5 -- 
-- a) Si se quisiera agregar un nuevo power up, un misil teledirigido, que para poder activarlo se deba indicar el color 
-- del auto al que se quiere impactar, ¿la solución actual lo permite o sería necesario cambiar algo 
-- de lo desarrollado en los puntos anteriores? Justificar.

-- Sí se podria agregar sin problema como una función más misilTeledirigido :: Color -> PowerUp
-- y utilizarlo usarPoweUp (misilTeledirigido  "azul") "rojo" 

-- b) Si una carrera se conformara por infinitos autos, ¿sería posible usar las funciones del punto 1b y 1c 
-- de modo que terminen de evaluarse? Justificar.

-- La función del 1b si sería posible usarla "vaTranquilo" puede terminar sólo si el auto indicado no va tranquilo
-- (en este caso por tener a alguien cerca, si las condiciones estuvieran al revés, 
-- terminaría si se encuentra alguno al que no le gana).
-- Esto es gracias a la evaluación perezosa, any es capaz de retornar True si se encuentra alguno que cumpla 
-- la condición indicada, y all es capaz de retornar False si alguno no cumple la condición correspondiente. 
-- Sin embargo, no podría terminar si se tratara de un auto que va tranquilo.

-- para la funcion "puesto" no puede terminar nunca porque hace falta saber cuántos le van ganando, entonces por más 
-- que se pueda tratar de filtrar el conjunto de autos, nunca se llegaría al final para calcular la longitud

