
data Persona = Persona{
    nombre :: String,
    edad :: Int
} deriving(Show,Eq)

adolescente :: Persona -> Bool
adolescente persona = edad persona >=12 && edad persona <=18

adolescente' :: Persona -> Bool
adolescente' = (\anios -> anios>=12 && anios<=18).edad
--edad :: Persona -> Num y anios:: Num-> Bool

--Dado dos string, retornar el de mayor long.

mayorLong :: String -> String -> String
mayorLong s1 s2 
    | length s1 > length s2 = s1
    | otherwise = s2


elDeMayor ponderacion x y
    | poderacion x > ponderacion y = x
    | otherwise = y


type Alimento = String

agregarAlimento :: Alimento -> [Alimento] -> [Alimento]
agregarAlimento nuevoAlimento listaCompras = nuevoAlimento : listaCompras

cantidadAlimentos :: [Alimento] -> Int
cantidadAlimentos listaCompras = length listaCompras

yaEstaEnLaLista :: Alimento -> [Alimento] -> Bool
yaEstaEnlista alimento listaCompras = elem alimento listaCompras

        