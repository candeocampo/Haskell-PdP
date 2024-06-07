
data Elemento = Elemento { 
    tipo :: String,
    ataque :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje)
}


data Personaje = Personaje { 
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
}

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anioNuevo personaje = personaje{anioPresente=anioNuevo}

meditar ::  Personaje -> Personaje
meditar personaje = personaje{salud=dominioSalud (salud personaje/2) (salud personaje)}

dominioSalud :: Float -> Float -> Float 
dominioSalud nuevaSalud salud 
    | nuevaSalud + salud > 0 = nuevaSalud + salud
    | otherwise = 0

causarDanio :: Float -> Personaje -> Personaje
causarDanio saludDada personaje = personaje{salud= dominioSalud (-saludDada) (salud personaje)}

causarDanio' :: Float -> Personaje -> Personaje
causarDanio' valorBajar  = modificarSalud (max 0 . flip (-) valorBajar) 

meditar' :: Personaje -> Personaje 
meditar' personaje = modificarSalud (*1.5) personaje

modificarSalud :: (Float -> Float) -> Personaje -> Personaje
modificarSalud transformacion personaje = personaje{salud=(transformacion.salud) personaje}

-- PUNTO 2)

esMalvado :: Personaje -> Bool
esMalvado personaje = any(tieneElemento "maldad")(elementos personaje) 

tieneElemento :: String -> Elemento -> Bool
tieneElemento maldad elemento = tipo elemento == maldad

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personajeInicial elemento = ((salud personajeInicial -).salud.(ataque elemento)) personajeInicial

type Enemigos = [Personaje]

enemigosMortales :: Personaje -> Enemigos -> Enemigos
enemigosMortales personaje enemigos = filter (enemigoMortal personaje) enemigos

enemigoMortal :: Personaje -> Personaje -> Bool 
enemigoMortal personaje enemigo = any (ataqueMortal personaje) (elementos enemigo)

ataqueMortal :: Personaje -> Elemento -> Bool
ataqueMortal personaje elemento = (muere.(ataque elemento)) personaje

muere :: Personaje -> Bool
muere = ((==0)).salud

-- PUNTO 3)
--3.a)
noHaceNada = id

concentracion :: Int -> Elemento
concentracion cantConcentracion = Elemento {
    tipo = "magia", 
    ataque = noHaceNada, 
    defensa = (!! cantConcentracion) .  iterate meditar'
}

--3.b)
esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esBirro

esBirro :: Elemento
esBirro = Elemento{
    tipo = "maldad",
    ataque = causarDanio' 1,
    defensa = noHaceNada
}

--3.c)
jack :: Personaje
jack = Personaje{
    nombre = "Jack",
    salud = 300,
    elementos = [concentracion 3,katanaMagica],
    anioPresente = 200
}

katanaMagica :: Elemento
katanaMagica = Elemento{
    tipo = "magia",
    ataque = noHaceNada,
    defensa = causarDanio' 100
}


















