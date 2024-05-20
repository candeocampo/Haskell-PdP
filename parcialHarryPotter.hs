data Postre = UnPostre{
    sabores :: [Sabores],
    peso :: Int,
    temperatura :: Int
} deriving(Show,Eq)

type Sabores = String

--Ejemplos
bizcocho :: Postre
bizcocho = UnPostre ["Borracho de Fruta","Crema"] 100 25

type Hechizo = Postre -> Postre

cambiarPesoPostre :: Int -> Hechizo
cambiarPesoPostre valor postre = UnPostre{ peso = (peso postre * (100 - valor)) `div` 100 }
cambiarTemperaturaPostre :: Int -> Hechizo
cambiarTemperaturaPostre nuevaTemp postre = UnPostre{temperatura=nuevaTemp}

agregarNuevoSabor :: String -> Postre -> Postre
agregarNuevoSabor nuevoSabor postre = UnPostre{sabores=nuevoSabor:sabores postre}

perderSabores :: Hechizo
perderSabores postre = UnPostre{sabores=[]}

incendio :: Hechizo
incendio = cambiarTemperaturaPostre 1 . cambiarPesoPostre 5

inmmobulus:: Hechizo
inmmobulus = cambiarTemperaturaPostre 0

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarNuevoSabor "Concentrado" . cambiarPesoPostre 10

diffindo :: Int -> Hechizo
diffindo porcentaje = cambiarPesoPostre porcentaje

riddikulus :: String -> Hechizo
riddikulus saborNuevo = agregarNuevoSabor (reverse saborNuevo)

avadaKedavra :: Hechizo
avadaKedavra = inmmobulus . perderSabores