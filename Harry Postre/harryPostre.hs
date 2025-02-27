module Lib where
import PdePreludat

data Postre = Postre{
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
}deriving(Show,Eq)

type Sabor = String

bizcocho:: Postre 
bizcocho = Postre ["borracho","fruta","crema"] 100 25 

tarta :: Postre 
tarta = Postre ["melaza"] 0 50

-- PUNTO B
type Hechizo = Postre -> Postre

modificarTemperatura :: Number -> Hechizo 
modificarTemperatura temp postre = postre{temperatura=temp}

calentarPostre :: Number -> Hechizo 
calentarPostre temp postre = postre{temperatura=temperatura postre + temp}

modificarPesoEnPorcentaje :: Number -> Hechizo
modificarPesoEnPorcentaje porcentaje postre = postre{peso= peso postre * (100-porcentaje) `div` 100}

incendio :: Hechizo 
incendio  = calentarPostre 1 . modificarPesoEnPorcentaje 5

immobulus :: Hechizo 
immobulus = modificarTemperatura 0 

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado" . modificarPesoEnPorcentaje 10

agregarSabor :: String -> Hechizo 
agregarSabor nuevoPostre postre = postre{sabores=nuevoPostre:sabores postre}

diffindo :: Number ->  Hechizo 
diffindo valor = modificarPesoEnPorcentaje  valor

riddikulus :: String -> Hechizo
riddikulus nuevoSabor = agregarSabor (reverse nuevoSabor)

avadaKedavra :: Hechizo 
avadaKedavra = modificarTemperatura 0 . perderSabores

perderSabores :: Postre -> Postre 
perderSabores postre = postre{sabores=[]}

-- PUNTO C
estaranListos :: Hechizo -> [Postre] -> Bool 
estaranListos hechizo = all (postreListo.hechizo)

postreListo :: Postre -> Bool 
postreListo postre = not(postreCongelado postre) && (>0) (peso postre) && not(null(sabores postre))

postreCongelado :: Postre -> Bool  
postreCongelado  = (==0).temperatura

-- PUNTO D 
postresListos :: [Postre] -> Number 
postresListos  = promedio . map peso . filter postreListo

promedio :: [Number] -> Number 
promedio nums = sum nums / length nums

-- PARTE 2 --

-- PUNTO A 
data Mago = Mago{
    hechizosAprendidos :: [Hechizo],
    horrorcrux :: Number 
}deriving (Show)

asistirClaseDefensa :: Hechizo -> Postre -> Mago -> Mago 
asistirClaseDefensa hechizo postre = agregarHechizo hechizo . agregarHorrorcrux hechizo postre

agregarHorrorcrux ::  Hechizo -> Postre -> Mago -> Mago 
agregarHorrorcrux hechizo postre mago 
    | hechizo postre == avadaKedavra postre = mago{horrorcrux=horrorcrux mago + 1}
    | otherwise = mago

agregarHechizo :: Hechizo -> Mago -> Mago 
agregarHechizo hechizo mago = mago{hechizosAprendidos=hechizo:hechizosAprendidos mago}

-- PUNTO B 
-- Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al
-- postre con más cantidad de sabores luego de usarlo.

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (elMejorEntre postre) (hechizosAprendidos mago)

elMejorEntre  :: Postre -> Hechizo -> Hechizo -> Hechizo
elMejorEntre postre hechizo1 hechizo2 
    | esMejor postre hechizo1 hechizo2 = hechizo1 
    | otherwise = hechizo2 

esMejor :: Postre -> Hechizo -> Hechizo -> Bool 
esMejor postre hechizo1 hechizo2 =  (length.sabores. hechizo1) postre > (length.sabores. hechizo2) postre

-- con recursividad:
mejorHechizo' :: Postre -> Mago -> Hechizo 
mejorHechizo' postre mago = elMejorEntre' postre (hechizosAprendidos mago)

elMejorEntre' :: Postre -> [Hechizo] -> Hechizo
elMejorEntre' _ [hechizo] = hechizo
elMejorEntre' postre (hechizo1:hechizo2:hechizos) 
    | (length.sabores. hechizo1) postre > (length.sabores. hechizo2) postre = elMejorEntre' postre (hechizo1:hechizos)
    | otherwise = elMejorEntre' postre (hechizo2:hechizos)

-- PARTE 3 -- 
-- PUNTO A) 
postreInfinito ::  [Postre] 
postreInfinito = repeat bizcocho

magoInfinitoHechizos :: Mago
magoInfinitoHechizos = Mago{
    hechizosAprendidos = repeat avadaKedavra,
    horrorcrux = 10
}

--B) Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos
-- ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar
-- conceptualmente.

-- Verdadero, existe la consulta:
-- Prelude> estaranListos avadaKedabra postreInfinito
-- La ejecución devolvera falso pues devido a la evaluación diferida, el all cuando encuentra el primer
-- postre que no está listo ya retorna, y no requiere construir la lista infinita.


-- C) Suponiendo que un mago tiene infinitos hechizos ¿Existe algún caso en el que se puede
-- encontrar al mejor hechizo? Justificar conceptualmente.

{-
No existe ninguna forma de conocer el mejor hechizo del mago porque para hacerlo 
hay que evaluar todos los elementos lista, aún teniendo lazy evaluation.
-}






















