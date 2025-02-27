module Lib where
import PdePreludat

data Genero = Genero{
    nombreGenero :: String,
    cantidadSeguidores :: Number,
    nivelDeDescontrolQueGenera :: Number
}deriving(Show,Eq)

rock = Genero "rock" 311250 3
pop = Genero "pop" 53480 1
electronico = Genero "electronico" 10000 4
hipHop = Genero "hip-hop" 2000 1

data Banda = Banda{
    nombre :: String,
    seguidores :: Number,
    estilo :: [Genero]
}deriving(Show,Eq)

cuadraditos = Banda {
    nombre = "emilioPrincipeYSusCuadraditosDeDulceDeLeche",
    seguidores = 7500,
    estilo = [rock]
}
aguaSinGas = Banda{
    nombre = "aguaSinGasDolbySurround",
    seguidores = 61815,
    estilo = [rock, pop]
}
djCutaneo = Banda{
    nombre = "djCutaneoInTheMix",
    seguidores = 2000,
    estilo = [electronico, hipHop]
}

type Festival = [Banda]
cabalgataMusic = [aguaSinGas, djCutaneo]
heinekerRock = [cuadraditos, aguaSinGas]

-- punto 1 --

-- punto a --
nivelDeDescontrol :: Genero -> Number
nivelDeDescontrol genero = (* nivelDeDescontrolQueGenera genero) . cantidadSeguidores $ genero

-- punto b --
porcentajeDeRelevancia :: Banda -> Genero -> Number
porcentajeDeRelevancia banda genero 
    | genero `elem` (estilo banda) = calcularPorcentaje banda genero
    | otherwise = 0

calcularPorcentaje :: Banda -> Genero -> Number 
calcularPorcentaje banda genero = (seguidores banda / sum (map cantidadSeguidores (estilo banda))) * 100

cantidadTotalSeguidores :: [Genero] -> Number
cantidadTotalSeguidores = sum . map cantidadSeguidores

-- punto c --
nivelDeDescontrolBanda :: Banda -> Number
nivelDeDescontrolBanda = sum . nivelesDeDescontrolBanda
-- otra forma = sum (map (nivelDeDescontrolGenero banda) (estilo banda))
-- Calcula el nivel de descontrol de un género para una banda
nivelDeDescontrolGenero  :: Banda -> Genero -> Number
nivelDeDescontrolGenero  banda genero = nivelDeDescontrol genero  * porcentajeDeRelevancia banda genero

nivelesDeDescontrolBanda :: Banda -> [Number]
nivelesDeDescontrolBanda banda = map (nivelDeDescontrolGenero  banda) (estilo banda)

-- punto 2.a --
mejorElemento :: Ord a => (b -> a) -> [b] -> b
mejorElemento f lista = foldl (mayorSegun f) (head lista) lista

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b 
    | f a > f b = a
    | otherwise = b

-- 2.b --
mejorGenero :: [Genero] -> Genero
mejorGenero = mejorElemento nivelDeDescontrolQueGenera 

-- Apa de 
generoMasTocado :: [Banda] -> [Genero] -> Genero
generoMasTocado bandas generos = mejorElemento (contarBandas bandas) generos

-- Función para contar cuántas bandas tocan un género
contarBandas :: [Banda] -> Genero -> Number
contarBandas bandas genero = length (filter (tocaGenero genero) bandas)

tocaGenero :: Genero -> Banda -> Bool
tocaGenero genero banda = genero `elem` estilo banda

{--
-- Cuenta cuántas bandas tocan un género
contarBandasPorGenero :: [Banda] -> Genero -> Number
contarBandasPorGenero bandas genero = length (filter (elem genero . estilo) bandas)

-- Punto 2.b
generoMasTocado :: [Banda] -> [Genero] -> Genero
generoMasTocado bandas = mejorElemento (contarBandasPorGenero bandas)

--}


-- punto 3 --
-- Saber cuál es el género principal de un festival, que es aquel que toca 
-- la mayor cantidad de bandas participantes.

generoPrincipalFestival :: Festival -> Genero
generoPrincipalFestival festival = generoMasTocado festival (generosFestival festival)

generosFestival :: Festival -> [Genero]
generosFestival = concat . map estilo 

-- punto 4 --
incorporarGeneros :: Banda -> Genero -> Banda
incorporarGeneros banda generoNuevo = Banda{
    seguidores = seguidores banda + agregarSeguidores generoNuevo,
    estilo = generoNuevo : estilo banda
}

agregarSeguidores :: Genero -> Number
agregarSeguidores genero = cantidadSeguidores genero / 1000





