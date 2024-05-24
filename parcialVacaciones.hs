
data Turista = Turista{
    cansansio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomasQueHabla :: [Idioma]
}deriving(Show,Eq)

type Idioma = String

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya = (bajarCansansio 5.bajarStress 1)

bajarCansansio :: Int -> Excursion
bajarCansansio valor turista = turista{cansansio=cansansio turista - valor}

bajarStress :: Int -> Excursion
bajarStress num turista = turista{stress=stress turista-num}

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje elemento = bajarStress (length elemento) 

salirAHablarIdioma :: String -> Excursion
salirAHablarIdioma idiomaNuevo turista = turista{idiomasQueHabla=idiomaNuevo:idiomasQueHabla turista,viajaSolo=not(viajaSolo turista)}




























