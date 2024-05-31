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

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
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

--PUNTO 1.a)
mayor :: Ord a => (b -> a) -> b -> b -> Bool
mayor f x y = f x > f y

menor :: Ord a => (b -> a) -> b -> b -> Bool
menor f x y = f x < f y 

--PUNTO 1.b)
--ordenarSegun (mayor (length)) (lista)

--PUNTO 2.a)
ubicadoEn :: [Barrio] -> Requisito
ubicadoEn listaBarrio depto = elem (barrio depto) listaBarrio

barrio1 :: [Barrio]
barrio1 = ["Palermo","Recoleta"]

--PUNTO 2.b)
cumpleRango :: (Depto -> Int )-> Int -> Int -> Requisito
cumpleRango funcion cotaSup contaInf = (between cotaSup contaInf.funcion)

--PUNTO 3.a)
cumpleBusqueda :: Busqueda -> Requisito 
cumpleBusqueda listaRequisitos depto = all (\req -> req depto) listaRequisitos

--cumpleRequisito :: Depto -> Requisito
--cumpleRequisito depto requisito = (requisito depto)
-- cumpleBusqueda listaRequisitos depto = all (cumpleRequisito depto) listaRequisitos










