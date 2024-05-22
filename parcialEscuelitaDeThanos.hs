
--PUNTO 1)

data Personaje=Personaje{
    edad :: Int,
    energia :: Float,
    habilidades :: [String],
    nombre :: String,
    planetaQueVive :: String
}deriving(Show,Eq)

data Guantalete = Guantalete{
    material :: String,
    gemasPosee :: [Gema]
}deriving(Show)

type Gema = Personaje -> Personaje

type Universo = [Personaje]

guanteleteCompleto :: Guantalete -> Bool
guanteleteCompleto guantalete = ((==6).length.gemasPosee) guantalete && material guantalete == "uru"

reducirMitadUniverso ::  Universo -> Universo
reducirMitadUniverso universo =  take (length universo  `div` 2) universo

chasquearDedos :: Guantalete -> Universo -> Universo
chasquearDedos guantalete universo 
    | guanteleteCompleto guantalete = reducirMitadUniverso universo
    | otherwise = universo

--PUNTO 2
edadPersonaje :: Personaje -> Bool
edadPersonaje = (<45).edad

universoAptoParaPendex :: Personaje -> Universo -> Bool
universoAptoParaPendex personaje universo = any (edadPersonaje) universo

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad  = (>1).length.habilidades

energiaTotalUniverso :: Universo -> Float
energiaTotalUniverso = sum.map energia.filter tieneMasDeUnaHabilidad
--          universo = sum (map energia (filter tieneMasDeUnaHabilidad universo))








