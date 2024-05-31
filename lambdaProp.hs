type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto{
    ambientes :: Int,
    superficie :: Int,
    precio :: Int,
    barrio :: Barrio
}deriving(Show,Eq)

data Persona = Personaje{
    mail :: Mail,
    busquedas :: [Busqueda]
} 

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = 
    (ordenarSegun criterio . filter(not.criterio x)) xs ++ 
    [x] ++
    (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor = 
    valor <= cotaSuperior && valor >= cotaInferior

deptosEjemplo :: [Depto]
deptosEjemplo = [
    Depto 3 80 7500 "Palermo",
    Depto 1 45 3500 "Villa Urquiza",
    Depto 2 50 5000 "Palermo",
    Depto 1 45 5500 "Recoleta"]

