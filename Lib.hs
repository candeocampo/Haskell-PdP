module Lib where
import PdePreludat


-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

-------------------------------------------
---- Resolución del ejercicio
----------------------------------------------
--1.a)
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro{velocidad=10,precision=precisionJugador habilidad * 2,altura=0}

madera :: Palo 
madera habilidad = UnTiro{velocidad=100,precision=precisionJugador habilidad `div` 2 ,altura=5}

hierros :: Number -> Palo
hierros n habilidad = UnTiro{velocidad=fuerzaJugador habilidad *n,precision=precisionJugador habilidad `div` n,altura=max 0 (n-3)}

--1.b)
type Palos = [Palo]

palos :: Palos
palos = [putter,madera] ++ map hierros [1..10]

--2)
golpe :: Palo -> Jugador -> Tiro
golpe palo = palo.habilidad

--3.a)
data Obstaculo = UnObstaculo{
  unTiroPuedeSuperarlo :: Tiro -> Bool,
  efectoTiroLuegoDeSuperar :: Tiro -> Tiro
}

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroPrincipal 
  | unTiroPuedeSuperarlo obstaculo tiroPrincipal = efectoTiroLuegoDeSuperar obstaculo tiroPrincipal   
  | otherwise = tiroNulo

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo{
  unTiroPuedeSuperarlo = superaTunelConRampita,
  efectoTiroLuegoDeSuperar = efectoLuegoDeSuperarTunel
}

laguna :: Number -> Obstaculo
laguna alturaLaguna = UnObstaculo{
  unTiroPuedeSuperarlo = superaLaguna,
  efectoTiroLuegoDeSuperar =  efectoLuegoDeSuperarLaguna alturaLaguna
}

hoyo :: Obstaculo
hoyo = UnObstaculo{
  unTiroPuedeSuperarlo = superaHoyo,
  efectoTiroLuegoDeSuperar = efectoLuegoDeSuperarHoyo
}

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = (>90) (precision tiro)

efectoLuegoDeSuperarTunel :: Tiro -> Tiro
efectoLuegoDeSuperarTunel tiro = UnTiro{velocidad=velocidad tiro*2,precision=100,altura=0}

superaLaguna :: Tiro -> Bool 
superaLaguna tiro = velocidad tiro > 80 && between (altura tiro) 1 5
 
efectoLuegoDeSuperarLaguna :: Number -> Tiro -> Tiro
efectoLuegoDeSuperarLaguna alturaLaguna tiro = UnTiro{altura=(altura tiro`div`alturaLaguna)}

superaHoyo :: Tiro -> Bool
superaHoyo tiro = between (velocidad tiro) 5 20 && (>95) (precision tiro)

efectoLuegoDeSuperarHoyo :: Tiro -> Tiro
efectoLuegoDeSuperarHoyo tiro = tiroNulo

tiroNulo :: Tiro
tiroNulo = UnTiro 0 0 0 

--4.a)
{-
golpe :: Palo -> Jugador -> Tiro
golpe palo = palo.habilidad

type Palos = [Palos]
-}

palosUtiles :: Jugador -> Obstaculo -> Palos
palosUtiles jugador obstaculo = filter (paloSirveParaSuperarlo jugador obstaculo) palos 

paloSirveParaSuperarlo :: Jugador -> Obstaculo -> Palo -> Bool
paloSirveParaSuperarlo jugador obstaculo = unTiroPuedeSuperarlo obstaculo . flip golpe jugador

--4.b)
tiro1 = UnTiro 10 95 0 
lista = [tunelConRampita,tunelConRampita,hoyo]

obstaculosConsecutivosQueSupera :: Tiro -> [Obstaculo] -> Number
obstaculosConsecutivosQueSupera _ [] = 0
obstaculosConsecutivosQueSupera tiro (obstaculo:obstaculos)
  | unTiroPuedeSuperarlo obstaculo tiro = 1 + obstaculosConsecutivosQueSupera (efectoTiroLuegoDeSuperar obstaculo tiro) obstaculos
  | otherwise = 0 --Va 0 porque ya no superaria consecuntivamente
  -- usas efectoTiroLuegoDeSuperar porque sería OTRO tiro que usamos para la recursividad.

--4.c)
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (flip obstaculosConsecutivosQueSupera obstaculos . flip golpe jugador) palos

--5)

padresQuePierdenApuesta :: [(Jugador,Puntos)] -> [String]
padresQuePierdenApuesta puntosTorneo = (map (padre.niñoJugador) . filter (not.ganoTorneo puntosTorneo)) puntosTorneo

niñoJugador = fst
puntosGanados = snd

ganoTorneo :: [(Jugador,Puntos)] -> (Jugador,Puntos) -> Bool
ganoTorneo puntosTorneo puntajeUnNiño = 
  (all ((< puntosGanados puntajeUnNiño). puntosGanados).filter (/=puntajeUnNiño)) puntosTorneo





