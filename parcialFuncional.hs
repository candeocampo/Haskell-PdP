

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

{- mayorSegun f a b 
    | f a > f b = a 
    | otherwise = b
-}

deltaSegun ponderacion transformacion valor = abs ((ponderacion . transformacion) valor - ponderacion valor)


--Casos de Prueba
investigador1 :: Investigador
investigador1  = Investigador "Candela" 60 [item1] ["Perder","Morir"]

investigador2 :: Investigador 
investigador2 = Investigador "Tomas" 30 [item1,item2] ["Ganar","Perder"]

investigador3 :: Investigador
investigador3 = Investigador "Morena" 0 [] ["Jugar"]

item1 :: Item
item1 = Item "Libros Fisica" 150

item2 :: Item
item2 = Item "Fisica cuantica" 350

lista1 :: [Investigador]
lista1 = [investigador1,investigador2,investigador3]

lista2 :: [Investigador]
lista2 = [investigador1]

----------------
-- Resolución --
----------------

--1.a)
enloquezca :: Int -> Investigador -> Investigador
enloquezca valor investigador = investigador{cordura=dominioDeCordura (cordura investigador) valor}

dominioDeCordura :: Int -> Int -> Int
dominioDeCordura valor variacion
    | (>0)(valor + variacion) = valor + variacion
    | otherwise = 0

--1.b)
hallarItem :: Item -> Investigador -> Investigador
hallarItem agregarItem = enloquezca (valor agregarItem) . (incorporarItem agregarItem)

incorporarItem :: Item -> Investigador -> Investigador
incorporarItem buscarItem investigador = investigador{items=buscarItem:items investigador}

--2)
tieneItem :: Item -> [Investigador] -> Bool
tieneItem itemBuscar investigadores = any (nombreIndicado itemBuscar) investigadores

nombreIndicado :: Item -> Investigador -> Bool
nombreIndicado item investigador = any (esMismoNombre item) (items investigador)

esMismoNombre :: Item -> Item -> Bool
esMismoNombre item1 item2 = nombreItem item1 == nombreItem item2

--3)
liderInvestigador :: [Investigador] -> Investigador
liderInvestigador [investigador] = investigador
liderInvestigador (investigador1:investigador2:listaInvestigadores) = mayorSegun potencialInvestigador investigador1 investigador2

mayorSegun f a b 
    | cordura (f a) > cordura (f b) = a 
    | otherwise = b

potencialInvestigador :: Investigador -> Investigador
potencialInvestigador investigador 
    | (==0) (cordura investigador)= investigador{cordura=0}
    | otherwise = investigador{cordura=cordura investigador * (experiencia investigador) + maximoItems investigador}

maximoItems :: Investigador -> Int
maximoItems investigador = maximum(map valor (items investigador))

experiencia :: Investigador -> Int
experiencia investigador = 1 + 3 * length (sucesosEvitados investigador)

--4.a)







