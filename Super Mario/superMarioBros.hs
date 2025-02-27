module Library where
import PdePreludat

data Plomero = Plomero{
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    historialReparaciones :: [Reparacion],
    dinero :: Number
}deriving(Show,Eq)

data Herramienta = Herramienta{
    denominacion :: String,
    precio :: Number,
    material :: String
}deriving(Show,Eq)

--- PUNTO 1 ---
-- PLOMEROS
mario :: Plomero 
mario = Plomero "Mario" [llaveInglesa,martillo] [] 1200

wario :: Plomero 
wario = Plomero "Wario" (infinitasLlavesFrancesas llaveFrancesa) [] 50

-- HERRAMIENTAS
llaveInglesa :: Herramienta
llaveInglesa = Herramienta "Llave Inglesa" 200 "Hierro"

martillo :: Herramienta
martillo = Herramienta "Martillo" 20 "Madera"

llaveFrancesa :: Herramienta
llaveFrancesa = Herramienta "Llave Francesa" 1 "Hierro"

infinitasLlavesFrancesas :: Herramienta -> [Herramienta]
infinitasLlavesFrancesas herramienta = [aumentarPrecio precio herramienta | precio <- [0..]]

aumentarPrecio :: Number -> Herramienta -> Herramienta
aumentarPrecio n herramienta = herramienta{precio= precio herramienta + n}

--- PUNTO 2 ---
tieneHerramienta :: String -> Plomero -> Bool 
tieneHerramienta nombreH = elem nombreH . map denominacion . cajaDeHerramientas  

esMalvado :: Plomero -> Bool 
esMalvado = comienzaCon . nombre

comienzaCon :: String -> Bool
comienzaCon = (== "Wa") . take 2

puedeComprar :: Plomero -> Herramienta -> Bool
puedeComprar plomero herramienta = dinero plomero >= precio herramienta

--- PUNTO 3 ---
esBuenaHerramienta :: Herramienta -> Bool
esBuenaHerramienta herramienta = esMartillo herramienta || empuñaduraHierro herramienta

empuñaduraHierro :: Herramienta -> Bool 
empuñaduraHierro herramienta = ((=="Hierro").material)herramienta && precio herramienta >= 10000

esMartillo :: Herramienta -> Bool 
esMartillo herramienta = ((== "Martillo").denominacion) herramienta && tipoMaterialMartillo (material herramienta)

tipoMaterialMartillo :: String -> Bool
tipoMaterialMartillo material = (== "Madera") material || (== "Goma") material

--- PUNTO 4 ---
comprar :: Plomero -> Herramienta -> Plomero
comprar plomero herramienta 
    | puedeComprar plomero herramienta = agregarHerramienta herramienta plomero
    | otherwise = plomero

--- PUNTO 5 ---
data Reparacion = Reparacion{
    descripcion :: String,
    requerimiento :: Plomero -> Bool
}deriving(Show,Eq)

filtracionDeAgua :: Reparacion
filtracionDeAgua = Reparacion "Filtracion de agua" (tieneHerramienta "Llave Inglesa")

esDificil :: Reparacion -> Bool 
esDificil = (>100) . length . descripcion

presupuesto :: Reparacion -> Number
presupuesto = (*3) .length.descripcion

--- PUNTO 6 ---
agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion reparacion plomero = plomero{historialReparaciones=reparacion:historialReparaciones plomero}

hacerReparacion :: Plomero -> Reparacion -> Plomero 
hacerReparacion plomero reparacion
    | requerimiento reparacion plomero || esMalvadoConMartillo plomero = (adicionales reparacion . agregarReparacion reparacion) plomero{dinero=dinero plomero + presupuesto reparacion}
    | otherwise = plomero{dinero=dinero plomero + 100}

esMalvadoConMartillo :: Plomero -> Bool 
esMalvadoConMartillo plomero = esMalvado plomero && tieneHerramienta "Martillo" plomero

destornillador :: Herramienta 
destornillador = Herramienta "Destornillador" 0 "Plastico"

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta herramienta plomero = plomero{cajaDeHerramientas= herramienta : cajaDeHerramientas plomero}

adicionales :: Reparacion -> Plomero -> Plomero
adicionales reparacion plomero 
    | esMalvado plomero = agregarHerramienta destornillador plomero
    | (not.esMalvado) plomero && esDificil reparacion = plomero{cajaDeHerramientas= (filter(not.esBuenaHerramienta).cajaDeHerramientas) plomero}
    | otherwise = plomero{cajaDeHerramientas= (tail.cajaDeHerramientas)plomero}

--- PUNTO 7 --- 
jornadaDeTrabajo :: Plomero -> [Reparacion] -> Plomero
jornadaDeTrabajo plomero  = foldl hacerReparacion plomero

--- PUNTO 8 --- 
maximoSegun :: (Plomero -> Number) -> [Plomero] -> Plomero
maximoSegun _ [plomero] = plomero
maximoSegun cantidadReparaciones (plomero1:plomero2:plomeros) 
    | cantidadReparaciones plomero1 > cantidadReparaciones plomero2 = maximoSegun cantidadReparaciones (plomero1:plomeros)
    | otherwise = maximoSegun cantidadReparaciones (plomero2:plomeros)

cantidadReparaciones :: Plomero -> Number
cantidadReparaciones = length.historialReparaciones

plomerosDespuesDeJornadaLaboral :: [Reparacion] -> [Plomero] -> [Plomero]
plomerosDespuesDeJornadaLaboral reparaciones = map (flip jornadaDeTrabajo reparaciones) 

masReparador :: [Reparacion] -> [Plomero] -> Plomero
masReparador reparaciones  = maximoSegun cantidadReparaciones . plomerosDespuesDeJornadaLaboral reparaciones 

masAdinerado :: [Reparacion] -> [Plomero] -> Plomero
masAdinerado reparaciones = maximoSegun dinero . plomerosDespuesDeJornadaLaboral reparaciones

masInvirtio :: [Reparacion] -> [Plomero] -> Plomero
masInvirtio reparaciones  = maximoSegun (length.cajaDeHerramientas) . plomerosDespuesDeJornadaLaboral reparaciones






