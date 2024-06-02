

data Investigador = Investigador {
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

mayorSegun f a b 
    | f a > f b = a 
    | otherwise = b

deltaSegun ponderacion transformacion valor = abs ((ponderacion . transformacion) valor - ponderacion valor)


--1.a)
enloquezca :: Int -> Investigador -> Investigador
enloquezca valor investigador = investigador{cordura=dominioDeCordura (cordura investigador) valor}

dominioDeCordura :: Int -> Int -> Int
dominioDeCordura valor variacion
    | (>0)(valor + variacion) = valor + variacion
    | otherwise = 0

--1.b)
hallarItem :: Item -> Investigador -> Investigador
hallarItem agregarItem = (enloquezca.length . items . incorporarItem agregarItem)

incorporarItem :: Item -> Investigador -> Investigador
incorporarItem buscarItem investigador = investigador{items=buscarItem:items investigador}

--2)
tieneItem :: Item -> [Investigador] -> Bool
tieneItem nombreItem investigadores = filter (nombreIndicado nombreItem nombreBuscar) investigadores

nombreIndicado ::  Item -> String -> Bool
nombreIndicado item nombreBuscar = nombreItem item /= nombreBuscar








