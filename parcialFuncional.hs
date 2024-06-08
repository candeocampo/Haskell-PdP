
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

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: (Investigador -> Int) -> Investigador -> Investigador -> Investigador
mayorSegun f a b 
    | f a > f b = a
    | otherwise = b

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
aplicarPotencial investigador = (cordura investigador) * experienciaInvestigador investigador + valorMaximoitems (items investigador)

experienciaInvestigador :: Investigador -> Int
experienciaInvestigador = (1+).(3*).length.sucesosEvitados 

estaLoco :: Investigador -> Bool
estaLoco investigador = 0 == cordura investigador

valorMaximoitems :: [Item] -> Int
valorMaximoitems [] = 0
valorMaximoitems items = maximum  (map valor items)


--4) usar deltaSegun

deltaSegun ponderacion transformacion valor = abs ((ponderacion.transformacion) valor - ponderacion valor)

--4.a)
deltaEnLaCorduraTotal :: Int -> [Investigador] -> Int 
deltaEnLaCorduraTotal puntos =  sum. map (deltaSegun cordura (enloquezca puntos)) 

--4.b)














