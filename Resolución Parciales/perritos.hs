module Lib where
import PdePreludat

-- parte A --

data Perrito = Perrito{
    raza :: String,
    juguetes :: [Juguete],
    tiempoEnGuarderia :: Number,
    energia :: Number
}deriving(Show,Eq)

type Juguete = String

data Guarderia = Guarderia{
    nombre :: String,
    rutina :: [Actividad]
}deriving(Show,Eq)

data Actividad = Actividad{
    ejercicio :: Ejercicio,
    tiempoEjercicio :: Number
}deriving(Show,Eq)

type Ejercicio = Perrito -> Perrito

jugarEj :: Ejercicio
jugarEj = modificarEnergia (subtract 10) 

ladrarEj :: Number -> Ejercicio
ladrarEj ladridos perrito = perrito{energia = energia perrito + (ladridos `div` 2)}

regalarEj :: Juguete -> Ejercicio
regalarEj nuevoToy perrito = perrito{juguetes = nuevoToy : juguetes perrito}

modificarEnergia :: (Number -> Number) -> Perrito -> Perrito
modificarEnergia transformacion perrito = perrito{energia=(max 0.transformacion.energia)perrito}

razasExtravagantes = ["dalmata","pomerania"]

diaDeSpaEj :: Ejercicio 
diaDeSpaEj perrito 
    | ((<50).tiempoEnGuarderia) perrito || esDeRazaExtravagante (raza perrito) = regalarEj "peine de goma" . modificarEnergia (+ (100 - energia perrito)) $ perrito
    | otherwise = perrito

esDeRazaExtravagante :: String  -> Bool
esDeRazaExtravagante razaPerrito = elem razaPerrito razasExtravagantes

diaDeCampoEj :: Ejercicio
diaDeCampoEj perrito = perrito{juguetes=tail.juguetes $ perrito}

-- Perritos modelos
zara :: Perrito
zara = Perrito "dalmata" ["pelota","mantita"] 90 80

guarderiaPDePerritos :: Guarderia
guarderiaPDePerritos = Guarderia{
    nombre = "Guarderia P de Perritos",
    rutina = [jugar, ladrar,regalarPelota, diaDeSpa, diaDeCampo]
}
jugar :: Actividad
jugar = Actividad jugarEj 30

ladrar :: Actividad
ladrar = Actividad (ladrarEj 18) 20

regalarPelota :: Actividad
regalarPelota = Actividad (regalarEj "pelota") 0

diaDeSpa :: Actividad
diaDeSpa = Actividad diaDeSpaEj 120

diaDeCampo :: Actividad
diaDeCampo = Actividad diaDeCampoEj 720

-- parte B --
puedeEstarEnGuarderia :: Perrito -> Guarderia -> Bool
puedeEstarEnGuarderia perrito guarderia = tiempoEnGuarderia perrito > tiempoRutina guarderia

tiempoRutina :: Guarderia -> Number
tiempoRutina = sum . map tiempoEjercicio . rutina

perroResponsable :: Perrito -> Bool
perroResponsable  = (>3). length . juguetes . diaDeCampoEj 

realizarRutina :: Perrito -> Guarderia -> Perrito
realizarRutina perrito guarderia 
    | puedeEstarEnGuarderia perrito guarderia = hacerRutina perrito (rutina guarderia)
    | otherwise = perrito

hacerRutina :: Perrito -> [Actividad] -> Perrito
hacerRutina perrito = foldl (flip($)) perrito . map ejercicio 

perrosCansados :: Guarderia -> [Perrito] -> [Perrito] 
perrosCansados guarderia = filter perritoCansado . perritosHacenRutina guarderia

perritosHacenRutina :: Guarderia -> [Perrito] -> [Perrito]
perritosHacenRutina guarderia = map (flip realizarRutina guarderia)

perritoCansado :: Perrito -> Bool
perritoCansado = (<5).energia











