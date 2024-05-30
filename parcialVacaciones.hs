
data Turista = Turista{
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomasQueHabla :: [Idioma]
}deriving(Show,Eq)

type Idioma = String

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya = bajarCansansio 5.bajarStress 1

bajarCansansio :: Int -> Excursion
bajarCansansio valor turista = turista{cansancio=cansancio turista + valor}

bajarStress :: Int -> Excursion
bajarStress num turista = turista{stress=stress turista-num}

cambiarStressPorcentual :: Int -> Turista -> Turista
cambiarStressPorcentual porciento turista = bajarStress (div (porciento * stress turista) 100) turista

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje elemento = bajarStress (length elemento) 

salirAHablarIdioma :: String -> Excursion
salirAHablarIdioma idiomaNuevo  = agregarIdioma idiomaNuevo . turistaAcompañado

agregarIdioma :: String -> Excursion
agregarIdioma idiomaNuevo turista =turista{idiomasQueHabla=idiomaNuevo:idiomasQueHabla turista}

turistaAcompañado :: Excursion
turistaAcompañado turista = turista{viajaSolo=False}

caminar ::  Int -> Excursion
caminar minutos  = aumentarCansansio (nivelDeIntensidad minutos).bajarStress (nivelDeIntensidad minutos)

nivelDeIntensidad :: Int -> Int
nivelDeIntensidad minutos = div minutos 4 

aumentarCansansio :: Int -> Excursion
aumentarCansansio valor turista = turista{cansancio=cansancio turista + valor}

paseoEnBarco :: String -> Excursion
paseoEnBarco marea turista
    | marea == "fuerte" = (bajarStress 6.aumentarCansansio 10) turista
    | marea == "moderada" = turista
    | marea == "tranquila" = (caminar 10 . apreciarElementoDelPaisaje "mar" . salirAHablarIdioma "aleman") turista

--PUNTO 1

ana :: Turista
ana =
  Turista { cansancio = 0 , stress = 20, viajaSolo = False, idiomasQueHabla = ["espaniol"] }

beto :: Turista
beto =
  Turista { cansancio = 15, stress = 15, viajaSolo = True, idiomasQueHabla = ["aleman"] }

cathi :: Turista
cathi =
  Turista { cansancio = 15, stress = 15, viajaSolo = True, idiomasQueHabla = ["aleman", "catalan"] }

--PUNTO 2a)
hacerExcursion :: Excursion -> Turista -> Turista  
hacerExcursion excursion turista  = (cambiarStressPorcentual (10).excursion) turista

--PUNTO 2b)
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int)-> Turista -> Excursion -> Int
deltaExcursionSegun funcion turista excursion = deltaSegun funcion (hacerExcursion excursion turista) turista

--PUNTO 2c)
--i)

excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa turista = (>0).deltaExcursionSegun (length.idiomasQueHabla) turista

--ii)
excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista listaExcursiones = filter (esDesestresante turista) listaExcursiones

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista

--PARTE 3)

type Tour = [Excursion]
type Marea = String

completo :: Tour
completo = [caminar 20, apreciarElementoDelPaisaje "cascada", caminar 40, irALaPlaya ,agregarIdioma "melcacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco "aguas tranquilas", excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina mareaVecina =  [paseoEnBarco "mareaVecina", excursionEnIslaVecina mareaVecina, paseoEnBarco "mareaVecina"]

excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina "Fuerte" = apreciarElementoDelPaisaje "lago"
excursionEnIslaVecina _  = irALaPlaya


--3.a)











