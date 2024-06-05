import Data.Char (isUpper)
import Data.List (genericLength)


data Plomero = Plomero{
    nombre :: String,
    cajaDeHerramientas :: Herramientas,
    historialReparaciones :: Reparaciones,
    dinero :: Float
}deriving(Show,Eq)

type Herramientas = [Herramienta]


data Herramienta = Herramienta{
    nombreHerramienta :: String,
    mango :: String,
    valor :: Float
}deriving(Show,Eq)


-- HERRAMIENTAS
llaveInglesa :: Herramienta
llaveInglesa = Herramienta "llave inglesa" "hierro" 200

martillo :: Herramienta
martillo = Herramienta "martillo" "madera" 20

llaveFrancesa :: Herramienta
llaveFrancesa = Herramienta "Llave Francesa" "Hierro" 1

destonillador :: Herramienta
destonillador = Herramienta "Destonillador" "Plastico" 0

-- PERSONAJES
mario :: Plomero
mario = Plomero "Mario" [llaveInglesa, martillo] [] 1200

tomi :: Plomero
tomi = Plomero "Tomas" [martillo] [] 850

wario :: Plomero
wario = Plomero "Wario" (llavesInfinitas llaveFrancesa) [] 0.50


-- PUNTO 1
llavesInfinitas :: Herramienta -> Herramientas
llavesInfinitas herramienta = [aumentarValorHerramienta precio herramienta | precio <- [0..]]

aumentarValorHerramienta :: Float -> Herramienta ->  Herramienta
aumentarValorHerramienta valorNuevo herramienta  = herramienta{valor= valor herramienta + valorNuevo}

-- PUNTO 2
-- 2.a)
tieneHerramienta :: String -> Plomero -> Bool
tieneHerramienta buscarHerramienta plomero = any (encontrarHerramienta buscarHerramienta) (cajaDeHerramientas plomero)

encontrarHerramienta :: String -> Herramienta -> Bool
encontrarHerramienta buscarH herramienta = nombreHerramienta herramienta == buscarH

--2.b)
esMalvado :: Plomero -> Bool
esMalvado plomero = ((== "wa").tomarNletrasNombre 2) (nombre plomero)
--                = take 2 (nombre plomero) == "wa"

tomarNletrasNombre :: Int -> String -> String
tomarNletrasNombre = take

--2.c)
puedeComprar :: Plomero -> Herramienta -> Bool
puedeComprar plomero herramienta = dinero plomero >= valor herramienta

-- PUNTO 3)
herramientaBuena :: Herramienta -> Bool
herramientaBuena herramienta = empuñaduraGomaOMadera herramienta || empuñaduraPrecio herramienta

type Requisito = Herramienta -> Bool

empuñaduraGomaOMadera :: Requisito
empuñaduraGomaOMadera (Herramienta "martillo" "madera"_) = True
empuñaduraGomaOMadera (Herramienta "martillo" "goma" _) = True
empuñaduraGomaOMadera (Herramienta _ _ _ ) = False

empuñaduraPrecio :: Requisito
empuñaduraPrecio (Herramienta _ "hierro" precio) = precio >= 10000
empuñaduraPrecio _ = False

-- PUNTO 4)
comprarHerramienta :: Plomero -> Herramienta -> Plomero
comprarHerramienta plomero herramienta 
    | puedeComprar plomero herramienta = comprar (valor herramienta) herramienta plomero
    | otherwise = plomero

agregarHerramienta :: Herramienta -> Herramientas -> Herramientas
agregarHerramienta nuevaHerramienta cajaHerramientas = nuevaHerramienta:cajaHerramientas

type FuncionCaja = Herramientas -> Herramientas

actualizarCajaHerramientas :: FuncionCaja -> Plomero -> Plomero
actualizarCajaHerramientas funcionAplicar plomero = plomero{cajaDeHerramientas= funcionAplicar (cajaDeHerramientas plomero)}

actualizarDinero :: Float -> Plomero -> Plomero 
actualizarDinero precioPagar plomero = plomero{dinero= dinero plomero - precioPagar}

comprar :: Float -> Herramienta -> Plomero -> Plomero
comprar precio herramienta plomero = ((actualizarDinero precio). actualizarCajaHerramientas (agregarHerramienta herramienta)) plomero

--PUNTO 5)
-- 5.a)
data Reparacion = Reparacion{
    descripcion :: String,
    requerimiento :: Condicion
}
type Condicion = Plomero -> Bool

--filtracionAgua :: Reparacion
--filtracionAgua = Reparacion "filtracion de agua" tieneHerramienta ("llave inglesa")

-- 5.b)
reparacionDificil :: Reparacion -> Bool
reparacionDificil reparacion = tieneMasCaracteres (descripcion reparacion) && estaEnMayuscula (descripcion reparacion)

tieneMasCaracteres :: String -> Bool
tieneMasCaracteres nombreReparacion = length nombreReparacion > 100

estaEnMayuscula :: String -> Bool
estaEnMayuscula = all isUpper 

-- 5.c)
saberPresupuesto :: Reparacion -> Float
saberPresupuesto = (*3) . genericLength . descripcion


-- PUNTO 6)
{- cumpleCondicionesDeRepacion :: Reparacion -> Plomero -> Bool
cumpleCondicionesDeRepacion reparacion plomero = condicion reparacion plomero || esMalvadoConMartillo plomero

esMalvadoConMartillo :: Plomero -> Bool
esMalvadoConMartillo plomero = esMalvado plomero && tieneHerramienta "Martillo" plomero

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion reparacion plomero = plomero{historialReparaciones = reparacion : historialReparaciones plomero}

plomeroMalvado :: Plomero -> Bool
plomeroMalvado plomero = esMalvado plomero && tieneHerramienta "martillo" plomero

hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion reparacion  = actualizarDinero 100 . agregarReparacion reparacion . efectosColaterales reparacion

efectosColatares :: Reparacion -> Plomero -> Plomero 
efectosColatares reparacion plomero 
    | esMalvado plomero = actualizarCajaHerramientas (agregarHerramienta destonillador) plomero
    | reparacionDificil reparacion = actualizarCajaHerramientas (sacarBuenasHerramientas) plomero
    | otherwise =  actualizarCajaHerramientas sacarHerramienta plomero

sacarBuenasHerramientas :: Herramientas -> Herramientas
sacarBuenasHerramientas herramientas = filter (not.herramientaBuena) herramientas

sacarHerramienta :: Herramientas -> Herramientas
sacarHerramienta herramienta = tail herramienta
-} 

-- PUNTO 8

type PlomeroCriterio = Plomero -> Bool

type Plomeros = [Plomero]

type Reparaciones = [Reparacion]

hacerUnaJornadaLaboral :: Reparaciones -> Plomero -> Plomero
hacerUnaJornadaLaboral reparaciones plomero = foldl intentarHacerReparacion plomero reparaciones

empleadosDespuesDeUnaJornadaLaboral :: Plomeros -> Reparaciones -> Plomeros
empleadosDespuesDeUnaJornadaLaboral plomeros reparaciones = map (hacerUnaJornadaLaboral reparaciones) plomeros


mejorPlomero :: PlomeroCriterio -> [Plomero] -> Plomero
mejorPlomero _ [plomero] = plomero
mejorPlomero criterio (plomero1:plomero2:plomeros) 
    | criterio plomero1 > criterio plomero2 = mejorPlomero criterio (plomero1:plomeros)
    | otherwise = mejorPlomero criterio (plomero2:plomeros)

plomeroMasReparador :: PlomeroCriterio
plomeroMasReparador plomero = (cuantoTiene .historialReparaciones) plomero

cuantoTiene :: [a] -> Float
cuantoTiene lista = genericLength lista 

empleadoMasReparador :: Plomeros -> Reparaciones -> Plomero
empleadoMasReparador plomeros reparaciones = mejorPlomero plomeroMasReparador (empleadosDespuesDeUnaJornadaLaboral (flip plomeros reparaciones))
