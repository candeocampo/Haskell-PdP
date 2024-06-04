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

data Persona = Persona{
    mail :: Mail,
    busquedas :: [Busqueda]
} 

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
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto listaRequisitos  = all (\req -> req depto) listaRequisitos

--cumpleRequisito :: Depto -> Requisito
--cumpleRequisito depto requisito = (requisito depto)
-- cumpleBusqueda listaRequisitos depto = all (cumpleRequisito depto) listaRequisitos

--PUNTO 3.b)

buscar :: Busqueda -> (Depto -> Depto -> Bool )-> [Depto] -> [Depto]
buscar busqueda criterio deptos = (ordenarSegun criterio.filter(flip cumpleBusqueda busqueda)) deptos

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = 
    (ordenarSegun criterio . filter(not.criterio x)) xs ++ 
    [x] ++
    (ordenarSegun criterio . filter (criterio x)) xs


-- PUNTO 4)
mailsDePersonasInterasadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInterasadas depto personas = (map mail . filter (estaInteresada depto)) personas

estaInteresada :: Depto -> Persona -> Bool
estaInteresada depto persona = any(cumpleBusqueda depto) (busquedas persona)








