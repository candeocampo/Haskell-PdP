
data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
}deriving (Show,Eq)

type Carrera = [Auto]

autoA :: Auto
autoA = Auto "Rosa" 100 150

-- PUNTO 1
--1.a)
estaCerca :: Auto -> Auto -> Bool 
estaCerca auto1 auto2 = auto1 /= auto2 &&  distanciaEntreAutos auto1 auto2 < 10

distanciaEntreAutos :: Auto -> Auto -> Int
distanciaEntreAutos auto1 auto2 = (abs.(distanciaRecorrida auto1 -) . distanciaRecorrida) auto2

--1.b)
vaTranquilo :: Auto -> Carrera -> Bool 
vaTranquilo auto carrera = not(tieneAlgunAutoCerca auto carrera) && vaGanando auto carrera

vaGanando :: Auto -> Carrera -> Bool 
vaGanando auto = all(leVaGanando auto).filter (/= auto)

tieneAlgunAutoCerca :: Auto -> Carrera -> Bool
tieneAlgunAutoCerca auto = any (estaCerca auto) 

leVaGanando :: Auto -> Auto -> Bool
leVaGanando ganador  = (< distanciaRecorrida ganador). distanciaRecorrida 

--1.c)
puesto :: Auto -> Carrera -> Int 
puesto auto = (+ 1).(length.filter (not.(leVaGanando auto))) 

-- PUNTO 2
-- 2.a)

type EstadoAuto = Auto -> Auto 

corra :: Int -> Auto -> Auto 
corra tiempo auto = auto{distanciaRecorrida=distanciaRecorrida auto + (tiempo *velocidad auto)}

--2.b)
type Modificador = Int -> Int 

modificador1 :: Modificador
modificador1 100 = 30

--i)
alterarVelocidad :: Modificador -> Auto -> Auto
alterarVelocidad modificador auto = auto{velocidad=(modificador.velocidad)auto}

--ii)
bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad valor auto = alterarVelocidad(max 0 . subtract valor) auto

-- PUNTO 3
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
    = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUps = Auto -> Carrera -> Carrera 

terremoto :: PowerUps
terremoto autoGatilla  = afectarALosQueCumplen (estaCerca autoGatilla) (bajarVelocidad 50) 

miguelitos :: Int -> PowerUps
miguelitos valor autoGatilla  = afectarALosQueCumplen (leVaGanando autoGatilla) (bajarVelocidad valor)

jetPack :: Int -> PowerUps
jetPack tiempo autoGatilla carrera = afectarALosQueCumplen (==autoGatilla) (alterarVelocidad (`div`2).corra tiempo. alterarVelocidad (*2)) carrera
 
-- PUNTO 4)

type Eventos = Carrera -> Carrera
type Color = String

--4.a)

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simularCarrera carrera eventos = (tablaDePosiciones . procesarEventos eventos) carrera

tablaDePosiciones :: Carrera -> [(Int, Color)]
tablaDePosiciones carrera = map(entradaDeTabla carrera) carrera

entradaDeTabla :: Carrera -> Auto -> (Int, Color)
entradaDeTabla carrera auto = (puesto auto carrera, color auto)

procesarEventos :: [Carrera -> Carrera] -> Carrera -> Carrera 
procesarEventos eventos carreraInicial = foldl (\carreraInicial eventos -> eventos carreraInicial) carreraInicial eventos

--4.b)
correnTodos:: Int -> Eventos
correnTodos tiempo carrera = map (corra tiempo) carrera

usaPowerUp :: PowerUps -> Color -> Eventos 
usaPowerUp powerup colorAuto carrera = powerup (find ((==colorAuto).color) carrera) carrera

find :: (c -> Bool) -> [c] -> c
find cond = head.filter cond




























































