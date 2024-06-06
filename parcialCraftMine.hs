
data Personaje = UnPersonaje {
nombre:: String,
puntaje:: Int,
inventario:: Materiales
} deriving Show

data Receta = UnaReceta{
    nombreReceta:: Material,
    listaMateriales :: Materiales,
    tiempoConstruccion :: Int
}deriving (Show)

type Material = String
type Materiales = [String]

jugador1 :: Personaje
jugador1 = UnPersonaje "Pepe" 1000 ["sueter","fogata","pollo","pollo"]

fogata :: Receta
fogata = UnaReceta "fogata" ["madera","fosforo"] 10

polloAsado :: Receta 
polloAsado = UnaReceta "pollo asado" ["fogata","pollo"] 300

sueter :: Receta 
sueter = UnaReceta "sueter" ["lana","agujas","tintura"] 600

-- Funciones para crafteear

type Craftear = Receta -> Personaje -> Personaje

craftearObjeto :: Craftear
craftearObjeto receta personaje 
    | cuentaConMaterialesRequeridos (listaMateriales receta) (inventario personaje) = agregarCambios receta personaje
    | otherwise = cambiarPuntaje (-100) personaje

cuentaConMaterialesRequeridos :: Materiales -> Materiales -> Bool
cuentaConMaterialesRequeridos materialesReceta materialesJugador = all (`elem` materialesReceta) materialesJugador

sacarMaterialesJugador :: Material -> Personaje -> Personaje
sacarMaterialesJugador material personaje = personaje{inventario=tomarMaterial material (inventario personaje)}

tomarMaterial :: Material -> Materiales -> Materiales
tomarMaterial _ [] = []
tomarMaterial material (material1:materiales)
    | material == material1 = materiales
    | otherwise = material : tomarMaterial material materiales

agregarMaterial :: Material -> Personaje -> Personaje 
agregarMaterial nuevoMaterial personaje = personaje{inventario=nuevoMaterial:inventario personaje}

cambiarPuntaje :: Int -> Personaje -> Personaje
cambiarPuntaje valor personaje = personaje{puntaje=puntaje personaje + valor}

agregarCambios :: Receta -> Personaje -> Personaje 
agregarCambios receta = cambiarPuntaje (10 * tiempoConstruccion receta). agregarMaterial (nombreReceta receta)

-- PUNTO 2
recetasDuplicanPuntaje :: Personaje -> [Receta] -> [Receta]
recetasDuplicanPuntaje personaje recetas = filter (flip duplicaPuntaje personaje) recetas

duplicaPuntaje :: Receta -> Personaje -> Bool
duplicaPuntaje receta jugador =  puntaje (craftearObjeto receta jugador) >= 2 * puntaje jugador

craftearObjetosSucesivamente :: Personaje -> [Receta] -> Personaje
craftearObjetosSucesivamente personaje recetas = foldl (flip craftearObjeto) personaje recetas

craftearAlReves :: Personaje -> [Receta] -> Bool
craftearAlReves personaje recetas =  puntaje (craftearObjetosSucesivamente personaje recetas) < puntaje (craftearObjetosSucesivamente personaje (reverse recetas))

-- PARTE "Mine"

data Bioma = UnBioma{
    nombreBioma :: String,
    biomaMaterial :: Materiales,
    condicionMinar :: Condicion
}

artico :: Bioma 
artico = UnBioma "Artico" ["hielo","iglues","lobos"] (tenerElemento "sueter")


type Condicion = Materiales -> Bool

type Herramienta = Bioma -> Material 

tenerElemento :: String -> Condicion 
tenerElemento elemento materiales = elem elemento materiales

hacha :: Herramienta
hacha bioma = last (biomaMaterial bioma)

espada :: Herramienta
espada bioma = head (biomaMaterial bioma)

pico :: Int -> Herramienta
pico n bioma = (biomaMaterial bioma) !! n 

minar :: Herramienta -> Personaje -> Bioma -> Personaje 
minar herramienta personaje bioma 
    | condicionMinar bioma (inventario personaje) = (cambiarPuntaje 50 . agregarMaterial (nombreBioma bioma)) personaje
    | otherwise = personaje 


--PUNTO 3
biomaInfinito :: Bioma
biomaInfinito = UnBioma "Infinito" (cycle ["oro", "plata", "bronce"]) (const True)

{- 
Hacha: Causa un ciclo infinito porque intenta evaluar el último elemento de una lista infinita.
Espada: Funciona correctamente, obteniendo siempre el primer elemento de la lista infinita.
Pico: Funciona correctamente siempre y cuando se especifique una posición dentro de los primeros elementos del ciclo.
Este comportamiento ilustra cómo las características de evaluación perezosa de Haskell permiten trabajar con listas infinitas, 
pero también cómo pueden surgir problemas con operaciones que intentan forzar la evaluación completa de tales listas.
--}







