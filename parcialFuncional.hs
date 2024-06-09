
data Investigador = Investigador{
    nombre :: String,
    cordura :: Int,
    items :: [Item],
    sucesosEvitados :: [String]
}deriving(Show,Eq)

data Item = Item{
    nombreItem :: String,
    valor :: Int
}deriving (Show,Eq)

--1.a)
modificarCordura :: (Int -> Int) -> Investigador -> Investigador
modificarCordura modificacion investigador = investigador{cordura= (modificacion.cordura) investigador}

enloquezca :: Int -> Investigador -> Investigador
enloquezca puntos = modificarCordura (max 0 . flip (-) puntos)

--1.b)
hallarItem :: Item -> Investigador -> Investigador
hallarItem  itemNuevo = enloquezca (valor itemNuevo) . incorporarItem itemNuevo

incorporarItem :: Item -> Investigador -> Investigador
incorporarItem itemNuevo investigador = investigador{items=itemNuevo:items investigador}

--2)
tieneItemAlgunInvestigador :: String -> [Investigador] -> Bool
tieneItemAlgunInvestigador nomItem = any (tieneItemInvestigador nomItem) 

tieneItemInvestigador :: String -> Investigador -> Bool
tieneItemInvestigador nomItem  = elem nomItem . map nombreItem . items

--3)
lider :: [Investigador] -> Investigador
lider investigadores = maximoSegun potencialInvestigador investigadores

potencialInvestigador :: Investigador -> Int
potencialInvestigador investigador 
    | not(estaLoco investigador) = aplicarPotencial investigador
    | otherwise = 0

aplicarPotencial :: Investigador -> Int 
aplicarPotencial investigador 
    | not(estaLoco investigador )= (cordura investigador) * experienciaInvestigador investigador + valorMaximoitems (items investigador)
    | otherwise = 0
experienciaInvestigador :: Investigador -> Int
experienciaInvestigador = (1+).(3*).length.sucesosEvitados 

estaLoco :: Investigador -> Bool
estaLoco investigador = 0 == cordura investigador

valorMaximoitems :: [Item] -> Int
valorMaximoitems [] = 0
valorMaximoitems items = maximum  (map valor items)

-------------------------------------------------
potencial :: Investigador -> Int
potencial investigador = (cordura investigador) * (1 + length (sucesosEvitados investigador)) + valorMaximoitems (items investigador)

lider' :: [Investigador] -> Investigador
lider' investigadores = maximoSegun aplicarPotencial investigadores

maximoSegun potencial = foldl1 (mayorSegun potencial) 

mayorSegun f a b 
    | f a > f b = a
    | otherwise = b

{-
mayorSegun f a b 
    | f a > f b = a
    | otherwise = b

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b 
    | f a > f b = a
    | otherwise = b

-}

--4) usar deltaSegun

deltaSegun ponderacion transformacion valor = abs ((ponderacion.transformacion) valor - ponderacion valor)

--4.a)
deltaEnLaCorduraTotal :: Int -> [Investigador] -> Int 
deltaEnLaCorduraTotal puntos =  sum. map (deltaSegun cordura (enloquezca puntos)) 

--4.b)
--deltaEnPotencial :: [Investigador] -> Int
--deltaEnPotencial [] = 0
--deltaEnPotencial (x:xs) = deltaSegun potencial (const 0) x + deltaEnPotencial xs

--4.c)
-- Para deltaCorduraTotal: Sí, es posible aplicar esta función a una lista infinita de investigadores. 
-- Esto se debe a que la función map procesa los elementos de la lista uno a uno de manera perezosa. 
-- La evaluación perezosa significa que los elementos de la lista se calcularán solo cuando sean necesarios, lo que permite procesar listas infinitas de manera eficiente.

-- Para deltaPotencialPrimerIntegrante: No es posible aplicar esta función a una lista infinita de investigadores. 
-- Esto se debe a que la función utiliza recursión sobre la lista de investigadores, lo que significa que intentará 
-- procesar todos los elementos de la lista. En una lista infinita, esta recursión nunca terminaría y la función no 
-- podría devolver un resultado.

{- data Suceso = Suceso{
    descripcion :: String,
    consecuencias :: [Investigador] -> Bool,
    evitarSuceso :: [Investigador] -> Investigador
}

-}

data Suceso = Suceso {
    descripcion :: String,
    condiciones :: [Investigador -> Bool],
    consecuencias :: [Investigador -> Investigador]
}

data GrupoInvestigadores = GrupoInvestigadores {
    integrantes :: [Investigador],
    liderI :: Investigador
}

-- Verifica si algún investigador cumple alguna de las condiciones para evitar el suceso
puedeEvitar :: [Investigador -> Bool] -> GrupoInvestigadores -> Bool
puedeEvitar condiciones grupo = any (\cond -> any cond (integrantes grupo)) condiciones

-- Aplica todas las consecuencias a cada investigador del grupo
aplicarConsecuencias :: [Investigador -> Investigador] -> GrupoInvestigadores -> GrupoInvestigadores
aplicarConsecuencias consecuencias grupo = grupo { integrantes = map (aplicarConsecuenciasInvestigador consecuencias) (integrantes grupo) }

aplicarConsecuenciasInvestigador :: [Investigador -> Investigador] -> Investigador -> Investigador
aplicarConsecuenciasInvestigador consecuencias investigador = foldl (\acc cons -> cons acc) investigador consecuencias

despertarDeUnAntiguo :: Suceso
despertarDeUnAntiguo = Suceso {
    descripcion = "Despertar de un antiguo",
    condiciones = [any (tieneItemInvestigador "Necronomicon") . integrantes],
    consecuencias = [
        \grupo -> grupo { integrantes = map (enloquezca 10) (integrantes grupo) },
        \grupo -> grupo { integrantes = if null (integrantes grupo) then [] else tail (integrantes grupo) }
    ]
}

ritualEnInnsmouth :: Suceso
ritualEnInnsmouth = Suceso {
    descripcion = "Ritual en Innsmouth",
    condiciones = [(\grupo -> any (\investigador -> potencial investigador > 100) (integrantes grupo))],
    consecuencias = [
        \grupo -> let (x:xs) = integrantes grupo in grupo { integrantes = hallarItem (Item "Daga maldita" 3) x : xs },
        \grupo -> grupo { integrantes = map (enloquezca 2) (integrantes grupo) },
        \grupo -> enfrentarSuceso despertarDeUnAntiguo grupo
    ]
}

enfrentarSuceso :: Suceso -> GrupoInvestigadores -> GrupoInvestigadores
enfrentarSuceso suceso grupo
    | puedeEvitar (condiciones suceso) grupo = grupo { integrantes = map (\i -> i { sucesosEvitados = descripcion suceso : sucesosEvitados i }) (integrantes grupo) }
    | otherwise = aplicarConsecuencias (consecuencias suceso) grupo

puedeEvitar :: [Investigador -> Bool] -> GrupoInvestigadores -> Bool
puedeEvitar condiciones grupo = any (\cond -> any cond (integrantes grupo)) condiciones

aplicarConsecuencias :: [GrupoInvestigadores -> GrupoInvestigadores] -> GrupoInvestigadores -> GrupoInvestigadores
aplicarConsecuencias consecuencias grupo = foldl (\acc cons -> cons acc) grupo consecuencias

-- 6. Función para que el grupo enfrente un suceso, enloqueciendo todos en 1 punto independientemente de si pueden evitarlo o no

enfrentarSucesoConPenalidad :: Suceso -> GrupoInvestigadores -> GrupoInvestigadores
enfrentarSucesoConPenalidad suceso grupo = enfrentarSuceso suceso grupo { integrantes = map (enloquezca 1) (integrantes grupo) }

-- 7. Calcular el delta en la cordura total del grupo luego de enfrentar el suceso

deltaCorduraTotal :: (GrupoInvestigadores -> GrupoInvestigadores) -> GrupoInvestigadores -> Int
deltaCorduraTotal funcion grupo = 
    let corduraOriginal = sum (map cordura (integrantes grupo))
        corduraFinal = sum (map cordura (integrantes (funcion grupo)))
   










