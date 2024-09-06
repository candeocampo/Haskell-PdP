module Library where
import PdePreludat

-- Punto 1 --
data Pokemon = Pokemon{
    nombre :: String,
    tipo :: String,
    nivel :: Number,
    fuerza :: Number,
    actividades :: [Actividad],
    habilidades :: [Habilidad]
}deriving(Show,Eq)

type Habilidad = String

pikachu :: Pokemon
pikachu = Pokemon "Pikachu" "electrico" 55 38 [caminadora 1 10] ["impactrueno","placaje electrico"]

snorlax :: Pokemon
snorlax = Pokemon "Snorlax" "normal" 30 80 [dormirLaSiesta 10, levantarPesas 30] ["bostezo","golpe cuerpo"]

-- Punto 2 --
type Actividad = Pokemon -> Pokemon

aumentarFuerza :: Number -> Pokemon -> Pokemon
aumentarFuerza valor pokemon = pokemon{fuerza= fuerza pokemon + valor}

disminuirNivel :: Number -> Pokemon -> Pokemon
disminuirNivel valor pokemon = pokemon{nivel = nivel pokemon - valor}

dormirLaSiesta :: Number -> Actividad
dormirLaSiesta horas pokemon 
    | horas < 5 = aumentarFuerza (10 * horas) pokemon
    | otherwise = disminuirNivel 1 pokemon

caminadora :: Number -> Number -> Actividad
caminadora minutos velocidad = aumentarFuerza ((minutos/15)*velocidad)

levantarPesas :: Number -> Actividad
levantarPesas pesoMancuerna pokemon 
    | ((<pesoMancuerna).fuerza)pokemon = aumentarFuerza pesoMancuerna pokemon
    | otherwise = disminuirFuerzaEnPorcentaje 10 pokemon

disminuirFuerzaEnPorcentaje :: Number -> Pokemon -> Pokemon
disminuirFuerzaEnPorcentaje porcentaje pokemon = pokemon{fuerza = fuerza pokemon - (fuerza pokemon * porcentaje)/100}

darUnPaseo :: Actividad
darUnPaseo pokemon = pokemon

-- Punto 3 --
type Rutina = Pokemon -> Pokemon

agregarHabilidad :: String -> Pokemon -> Pokemon
agregarHabilidad nuevoHab pokemon = pokemon{habilidades = nuevoHab : habilidades pokemon}

realizarActividades :: [Actividad] -> Pokemon -> Pokemon
realizarActividades actividades pokemon = foldl (flip($)) pokemon actividades

perderHabilidades :: Pokemon -> Pokemon
perderHabilidades pokemon = pokemon{habilidades=[]}

superentrenamiento :: Rutina
superentrenamiento pokemon = agregarHabilidad "patada alta" . realizarActividades (actividades pokemon) . caminadora 1 6 . levantarPesas 15 $ pokemon

rutinatranqui :: Rutina
rutinatranqui = perderHabilidades . dormirLaSiesta 2 . darUnPaseo

--Esta es inventada
nuevaRutina :: Rutina
nuevaRutina = levantarPesas 5 . agregarHabilidad "dormilon" . dormirLaSiesta 5

-- Punto 4.a --
realizarRutinas :: Pokemon -> [Rutina] -> Pokemon
realizarRutinas pokemon = foldl (flip($)) pokemon 

-- Punto 4.b --
pokemonesHacenRutina :: Rutina -> [Pokemon] -> [Pokemon]
pokemonesHacenRutina rutina = map rutina

-- Punto 4.c --
potentes :: Number -> [Rutina] -> [Pokemon] -> [Pokemon]
potentes n  rutinas = filter (esPotente n rutinas) 

esPotente :: Number -> [Rutina] -> Pokemon -> Bool
esPotente n rutinas pokemon =  n < (poderDeCombate . realizarRutinas pokemon) rutinas

poderDeCombate :: Pokemon -> Number
poderDeCombate pokemon = nivel pokemon + (fuerza pokemon/2)

-- Punto 4.d --
ordenados :: [Pokemon] -> Bool
ordenados [] = True
ordenados [pokemon] = True
ordenados (p1:p2:pokemones) = fuerza p1 <= fuerza p2 && ordenados (p2:pokemones) -- tiene que ser de forma creciente.

realizanPrimeroRutina :: Pokemon -> Pokemon
realizanPrimeroRutina = superentrenamiento . rutinatranqui

quedanOrdenadosSegunFuerza :: [Pokemon] -> Bool
quedanOrdenadosSegunFuerza  = ordenados . map realizanPrimeroRutina 

-- Punto 5.a --
pokemonInfinito :: Pokemon
pokemonInfinito = Pokemon "Infinito" "alien" 30 40 infinitosEjercicios  []

infinitosEjercicios :: [Actividad]
infinitosEjercicios = repeat darUnPaseo 

-- Punto 5.b--
-- ¿Se podria aplicar el pokemon del punto 5.a) en alguna e las funciones del parcial de manera que dicha función termine?
-- Si se puede dar ejemplos de invocación y respuesta, y justificar. Si no se puede justifique porque.

-- Si quisiera realizar las actividades del pokemon no me daría un resultado pues al ser una lista infinita, no terminaria de evaluar nunca.
-- porque foldl intentará procesar todos los elementos de la lista infinita.

-- La función realizarRutinas aplica una lista de rutinas a un Pokémon. Similar a realizarActividades, 
-- realizarRutinas intentará procesar todas las rutinas en el caso de una lista infinita de rutinas, 
-- por lo que no terminará si se usa una lista infinita de actividades.



