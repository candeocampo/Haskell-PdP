module Lib where
import PdePreludat

data Elemento = UnElemento { 
    tipo :: String,
	ataque :: (Personaje-> Personaje),
	defensa :: (Personaje-> Personaje) 
}deriving (Show,Eq)

data Personaje = UnPersonaje { 
    nombre :: String,
	salud :: Number,
	elementos :: [Elemento],
	anioPresente :: Number
}deriving(Show,Eq)

-- PUNTO 1
modificarSalud :: (Number -> Number) -> Personaje -> Personaje
modificarSalud transformacion personaje = UnPersonaje{salud= (transformacion.salud) personaje}

mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio nuevoAnio personaje = UnPersonaje{anioPresente=nuevoAnio}

meditar :: Personaje -> Personaje 
meditar personaje = modificarSalud (*1.5) personaje

causarDanio :: Number -> Personaje -> Personaje
causarDanio valor = modificarSalud (max 0 .flip (-) valor)

-- PUNTO 2
esMalvado :: Personaje -> Bool
esMalvado personaje = any ((/= "maldad").tipo) (elementos personaje)

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personajeInicial elemento = ((salud personajeInicial -) . salud . ataque elemento) personajeInicial

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje listaEnemigos = filter (enemigoMortal personaje) listaEnemigos

enemigoMortal :: Personaje -> Personaje -> Bool
enemigoMortal personaje enemigo = any (ataqueMortal personaje) (elementos enemigo)

ataqueMortal :: Personaje -> Elemento  -> Bool
ataqueMortal personaje elemento = (muere.ataque elemento) personaje 

muere :: Personaje -> Bool
muere = (==0).salud

-- PUNTO 3

noHacerNada = id

concentracion :: Number -> Elemento
concentracion nivelConcentracion = UnElemento{
    tipo = "Magia",
    ataque = noHacerNada,
    defensa = (!! nivelConcentracion) . iterate meditar
}

esBirros ::  Elemento
esBirros  = UnElemento{
    tipo = "Maldad",
    ataque = causarDanio 1,
    defensa = noHacerNada
}

esBirrosMalvado :: Number -> [Elemento]
esBirrosMalvado cantidad = replicate cantidad esBirros

katana :: Elemento
katana = UnElemento{
    tipo = "Magia",
    ataque = causarDanio 1000,
    defensa = noHacerNada
}

jack :: Personaje
jack = UnPersonaje{
    nombre = "Jack",
	salud = 300,
	elementos = [concentracion 3,katana],
	anioPresente = 200
}

portalAlFuturo :: Number -> Elemento
portalAlFuturo anio = UnElemento{
    tipo = "Magia",
    ataque = mandarAlAnio (anio+2800),
    defensa = mandarAlFuturo anio
}

aku :: Number -> Number -> Personaje
aku anioVive cantSalud = UnPersonaje{
    nombre = "Aku",
	salud = cantSalud,
	elementos = concentracion 4 : portalAlFuturo anioVive : esBirrosMalvado (anioVive*100),
	anioPresente = anioVive
}

mandarAlFuturo :: Number -> Personaje -> Personaje
mandarAlFuturo anio personaje = aku (anio+2800) (salud personaje)

-- PUNTO 4
{-
Finalmente queremos saber cómo puede concluir la lucha entre Jack y Aku. 
Para ello hay que definir la función luchar :: Personaje -> Personaje -> (Personaje, Personaje) 
donde se espera que si el primer personaje (el atacante) está muerto, 
retorne la tupla con el defensor primero y el atacante después, 
en caso contrario la lucha continuará invirtiéndose los papeles (el atacante será el próximo defensor) 
luego de que ambos personajes se vean afectados por el uso de todos los elementos del atacante.

Ejemplo:
O sea que si luchan Jack y Aku siendo Jack el primer atacante, 
Jack se verá afectado por el poder defensivo de la concentración y Aku se verá afectado 
por el poder ofensivo de la katana mágica, y la lucha continuará con Aku (luego del ataque) como atacante 
y con Jack (luego de la defensa) como defensor.
-}

luchar' :: Personaje -> Personaje -> (Personaje, Personaje)
luchar' atacante defensor 
    | muere atacante = (defensor,atacante)
    | otherwise = luchar' (usarElementos' ataque defensor (elementos atacante)) (usarElementos' defensa atacante (elementos defensor))

usarElementos' :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos' funcion personaje elementos  = foldl aplicarFuncion personaje (map (funcion) elementos)

aplicarFuncion personaje funcion = funcion personaje


{-
La función usarElementos toma tres argumentos:

(Elemento -> Personaje -> Personaje): Esto es el tipo de función que representa una acción que un elemento (como una habilidad o un ataque) tiene sobre un personaje. Esta función toma un elemento y un personaje como entrada, y devuelve un nuevo personaje que ha sido afectado por ese elemento.

Personaje: Este es el personaje sobre el que se aplicarán las acciones de los elementos.

[Elemento]: Esta es una lista de elementos que se aplicarán sobre el personaje.

-}





































