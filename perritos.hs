module Lib where
import PdePreludat

-- PARTE A 
data Perro = Perro{
    raza :: String,
    juguetesFavoritos :: [Juguete],
    tiempo :: Number,
    energia :: Number
}deriving(Show,Eq)

type Juguete = String

data Guarderia = Guarderia{
    nombre :: String,
    rutina :: [Actividad]
}deriving(Show,Eq)

data Actividad = Actividad{
    ejercicio :: Ejercicio,
    tiempoRutina :: Number
}deriving(Show,Eq)

type Ejercicio = Perro -> Perro

modificarEnergia :: (Number -> Number) -> Perro -> Perro 
modificarEnergia transformacion perro = perro{energia=(transformacion.energia)perro}

jugar :: Ejercicio
jugar  = modificarEnergia (max 0 . flip (-) 10) 

ladrar :: Number -> Ejercicio
ladrar ladrido = modificarEnergia (+ (ladrido `div` 2))

regalar :: String -> Ejercicio
regalar nuevoJuguete perro = perro{juguetesFavoritos=nuevoJuguete:juguetesFavoritos perro}

diaDeSpa :: Ejercicio
diaDeSpa perro 
    | tiempo perro >= 50 || (raza perro) `elem` razasExtravagante = (cambiarEnergia 100 .regalar "peine de goma") perro 
    | otherwise = perro

razasExtravagante :: [String]
razasExtravagante = ["dalmata","pomerania"]

cambiarEnergia :: Number -> Ejercicio 
cambiarEnergia valor perro = perro{energia=valor}

diaDeCampo :: Ejercicio
diaDeCampo  = perderJuguete . jugar

perderJuguete :: Ejercicio
perderJuguete perro = perro{juguetesFavoritos= tail (juguetesFavoritos perro)}

zara :: Perro
zara = Perro "dalmata" ["pelota","mantita"] 90 80

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = Guarderia{
    nombre = "Guarderia P de perritos",
    rutina = [Actividad jugar 30 ,Actividad (ladrar 18) 20 , Actividad (regalar "pelota") 0, Actividad diaDeSpa 120, Actividad diaDeCampo 720]
}

-- PARTE B

-- Saber si un perro puede estar en una guardería. 
-- Para eso, el tiempo de permanencia tiene que ser mayor que el de la rutina
puedeEstarEnGuarderia :: Perro -> Guarderia ->  Bool 
puedeEstarEnGuarderia perro guarderia = tiempo perro > (sum . map tiempoRutina . rutina) guarderia

-- Reconocer a perros responsables. Estos serían los que aún 
-- después de pasar un día de campo siguen teniendo más de 3 juguetes.

perrosResponsables :: Perro -> Bool
perrosResponsables = (>= 3). length . juguetesFavoritos . diaDeCampo

-- Que un perro realice una rutina de la guardería (que realice todos sus ejercicios). 
-- Para eso, el tiempo de la rutina no puede ser mayor al tiempo de permanencia. 
-- En caso de que esta condición no se cumpla, el perro no hace nada

realizarRutina :: Perro -> Guarderia -> Perro 
realizarRutina perro guarderia 
    | puedeEstarEnGuarderia perro guarderia = foldl (flip aplicarActividad) perro (rutina guarderia)
    | otherwise = perro

aplicarActividad :: Actividad -> Perro -> Perro 
aplicarActividad actividad perro = (ejercicio actividad) perro

-- Dados unos perros, reportar todos los que quedan cansados después de realizar la rutina de una guardería. 
-- Es decir, que su energía sea menor a 5 luego de realizar todos los ejercicios

perrosCansados :: [Perro] -> Guarderia -> [Perro]
perrosCansados perros guarderia = filter (flip quedaCansado guarderia) perros

perrosCansados' :: [Perro] -> Guarderia -> [Perro]
perrosCansados' perros guarderia = filter ((<5). energia . flip realizarRutina guarderia) perros

quedaCansado :: Perro -> Guarderia ->  Bool 
quedaCansado perro =  (< 5) . energia. realizarRutina perro

-- PARTE C
pi :: Perro
pi = Perro "labrador" infinitasSogitas 314 159

infinitasSogitas :: [Juguete]
infinitasSogitas =  map show [1..]





