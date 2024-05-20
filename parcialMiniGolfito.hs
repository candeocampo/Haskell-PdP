

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--PUNTO 1.a)
type Palo = Habilidad -> Tiro

--putter :: Habilidad -> Tiro

putter :: Palo
putter habilidad= UnTiro{velocidad=10,precision=precisionJugador habilidad*2,altura=0}

madera :: Palo
madera habilidad = UnTiro{velocidad=100,precision=precisionJugador habilidad `div` 2,altura=5}

hierros :: Int -> Palo
hierros n habilidad =  UnTiro{velocidad=fuerzaJugador habilidad*n,precision=precisionJugador habilidad `div` n,altura=n-3 `max` 0}

--PÚNTO 1.b)
palos :: [Palo]
palos = [putter,madera] ++ map hierros [1..10]

--PUNTO 2) 
golpe :: Jugador -> Palo -> Tiro
golpe  jugador palo = (palo.habilidad) jugador
-- golpe jugador palo = palo (habilidad jugador)


--PUNTO 3)

data Obstaculo = Obstaculo{
    puedeSuperarObstaculo :: Tiro -> Bool,
    efectoLuegoDeSuperar :: Tiro -> Tiro
}

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal
    | puedeSuperarObstaculo obstaculo tiroOriginal  = efectoLuegoDeSuperar obstaculo tiroOriginal
    | otherwise = tiroNulo

tunelConRampita :: Obstaculo
tunelConRampita = Obstaculo superaTunelConRampita efectoTunelConRampita

laguna :: Int -> Obstaculo
laguna alturaLaguna= Obstaculo superaLaguna (efectoLaguna alturaLaguna)

hoyo :: Obstaculo
hoyo = Obstaculo superaHoyo efectoHoyo

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && altura tiro == 0

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro{velocidad=velocidad tiro*2,precision=100,altura=0}
 
superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 .altura) tiro

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna alturaLaguna tiro=UnTiro{altura=altura tiro `div` alturaLaguna}

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20.velocidad) tiro && precision tiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroNulo

tiroNulo :: Tiro
tiroNulo = UnTiro 0 0 0

--PUNTO 4a)
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos 

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo = puedeSuperarObstaculo obstaculo.golpe jugador 


--puedeSuperarObstaculo  :: Tiro -> Bool y golpe :: Jugador -> Pal -> Tiro

--PUNTO 4b)
--BONUS:
--takeWhile :: (a -> Bool) -> [a] -> [a]

obstaculosConsecutivosQueSupera:: Tiro -> [Obstaculo] -> Int
obstaculosConsecutivosQueSupera tiro obstaculos = (length . takeWhile(\(obstaculo,tiroQueLlega)->puedeSuperarObstaculo obstaculo tiroQueLlega).zip obstaculos.tiroSucesivos tiro)obstaculos

tiroSucesivos :: Tiro -> [Obstaculo] -> [Tiro]
--tiroSucesivos tiroOriginal obstaculos= map (\ obstaculo-> efectoLuegoDeSuperar obstaculo tiroOriginal) obstaculos
tiroSucesivos tiroOriginal obstaculos = foldl (\tirosGenerados obstaculo -> tirosGenerados ++ [efectoLuegoDeSuperar obstaculo (last tirosGenerados)])[tiroOriginal] obstaculos

--DE FORMA RECURSIVA:
obstaculosConsecutivosQueSupera' :: Tiro -> [Obstaculo] -> Int
obstaculosConsecutivosQueSupera' tiro [] = 0
obstaculosConsecutivosQueSupera' tiro (obstaculo:obstaculos) 
  | puedeSuperarObstaculo obstaculo tiro = 1 + obstaculosConsecutivosQueSupera' (efectoLuegoDeSuperar obstaculo tiro) obstaculos
  | otherwise = 0 

--PUNTO 4c)
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (flip obstaculosConsecutivosQueSupera obstaculos.golpe jugador) palos


--PUNTO 5)

jugadorDeTorneo = fst
puntosGanados = snd

padresQuePierdenApuesta :: [(Jugador,Puntos)] -> [String]
padresQuePierdenApuesta puntosDeTorneo = (map(padre.jugadorDeTorneo).filter(not . gano puntosDeTorneo)) puntosDeTorneo

gano ::  [(Jugador,Puntos)] ->  [(Jugador,Puntos)] -> Bool
gano puntosDeTorneo puntosJugador 
  = (all((< puntosGanados puntosJugador).puntosGanados).filter (/= puntosJugador)) puntosDeTorneo