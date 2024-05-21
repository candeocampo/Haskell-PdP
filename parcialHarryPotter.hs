
data Postre = UnPostre{
    sabores :: [Sabores],
    peso :: Int,
    temperatura :: Int
} deriving(Show,Eq)

type Sabores = String

--Ejemplos
bizcocho :: Postre
bizcocho = UnPostre ["Borracho de Fruta","Crema"] 100 25

tarta :: Postre
tarta = UnPostre ["Melaza"] 50 0

--PUNTO B)

type Hechizo = Postre -> Postre

cambiarPesoPostre :: Int -> Hechizo
cambiarPesoPostre valor postre = postre{ peso = (peso postre * (100 - valor)) `div` 100 }

cambiarTemperaturaPostre :: Int -> Hechizo
cambiarTemperaturaPostre nuevaTemp postre = postre{temperatura=nuevaTemp}

agregarNuevoSabor :: String -> Postre -> Postre
agregarNuevoSabor nuevoSabor postre = postre{sabores=nuevoSabor:sabores postre}

perderSabores :: Hechizo
perderSabores postre = postre{sabores=[]}

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

--PUNTO C)
estaCongelado :: Postre -> Bool
estaCongelado = (>0).temperatura

unPostreListo :: Postre -> Bool
unPostreListo postre = peso postre > 0 && not(estaCongelado postre) && not(null (sabores postre))

estaraListoPostre :: Hechizo -> [Postre] -> Bool
estaraListoPostre hechizo listaPostre = all (unPostreListo.hechizo) listaPostre

--PUNTO D)
promedioPostre :: [Int] -> Int
promedioPostre pesos = (sum pesos)`div`(length pesos)

promedioPostresListos :: [Postre] -> Int
promedioPostresListos listaPostres= (promedioPostre . map peso .filter unPostreListo) listaPostres
--acá podrias simplificar lo de "listaPostres"

--PARTE 2) MAGOS
data Mago = UnMago{
    hechizosAprendidos :: [Hechizo],
    cantidadHorrorcruxex :: Int
}deriving Show

claseCocina :: Hechizo -> Postre -> Mago ->  Mago
claseCocina hechizo postre mago = sumarHorrocrux hechizo postre.practicaMago hechizo

practicaMago :: Mago -> Hechizo -> Mago
practicaMago mago hechizoPractica= mago{hechizosAprendidos=hechizoPractica:hechizosAprendidos mago}

esElMismoResultado :: Hechizo -> Postre -> Bool
esElMismoResultado hechizo postre = hechizo postre == avadaKedavra postre

sumarHorrocrux :: Mago
sumarHorrocrux mago = mago{cantidadHorrorcruxex=cantidadHorrorcruxex mago + 1}

sumarHorrocruxSegun :: Hechizo -> Postre -> Mago -> Mago
sumarHorrocruxSegun hechizo postre mago
    | esElMismoResultado hechizo postre = sumarHorrocrux mago
    | otherwise = mago