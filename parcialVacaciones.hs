
data Turista = Turista{
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomasQueHabla :: [Idioma]
}deriving(Show,Eq)

--Ejemplos 
ana :: Turista
ana =
  Turista { cansancio = 0 , stress = 20, viajaSolo = False, idiomasQueHabla = ["espaniol"] }

beto :: Turista
beto =
  Turista { cansancio = 15, stress = 15, viajaSolo = True, idiomasQueHabla = ["aleman"] }

cathi :: Turista
cathi =
  Turista { cansancio = 15, stress = 15, viajaSolo = True, idiomasQueHabla = ["aleman", "catalan"] }


type Idioma = String

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya = bajarCansansio 5.bajarStress 1

bajarCansansio :: Int -> Excursion
bajarCansansio valor turista = turista{cansancio=cansancio turista - valor}

bajarStress :: Int -> Excursion
bajarStress num turista = turista{stress=stress turista-num}

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
    | marea == "fuerte" = (bajarStress (-6).aumentarCansansio (10)) turista
    | marea == "moderada" = turista
    | marea == "tranquila" = (caminar 10 . apreciarElementoDelPaisaje "mar" . salirAHablarIdioma "aleman") turista



















