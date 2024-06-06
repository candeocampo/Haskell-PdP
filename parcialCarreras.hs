

data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
}deriving (Show,Eq)

type Carrera = [Auto]

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
corra tiempo auto = auto{distanciaRecorrida=(distanciaRecorrida auto + tiempo)*velocidad auto}

--2.b)









































































