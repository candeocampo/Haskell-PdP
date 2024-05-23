

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

energiaTotalUniverso :: Universo -> Int
energiaTotalUniverso = sum.map energia.filter tieneMasDeUnaHabilidad
--          universo = sum (map energia (filter tieneMasDeUnaHabilidad universo))


--PARTE 2

mente :: Float-> Gema
mente = quitarEnergia

quitarEnergia :: Float -> Gema 
quitarEnergia valor personaje = personaje{energia=energia personaje-valor}

alma :: String -> Gema 
alma habilidadBuscar personaje = (quitarEnergia 10.eliminarHabilidad habilidadBuscar) personaje

eliminarHabilidad :: String -> Gema
eliminarHabilidad habilidad personaje = personaje{habilidades=filter(/=habilidad)(habilidades personaje)}

espacio :: String -> Gema
espacio planetaNuevo personaje = (quitarEnergia 20.transportarRival planetaNuevo) personaje 

transportarRival :: String -> Gema
transportarRival nuevoPlaneta personaje = personaje{planetaQueVive=nuevoPlaneta}

poder :: Gema
poder personaje 
    | cantidadHabilidadesPersonaje personaje = personaje{energia=0,habilidades=[]}
    | otherwise = personaje

cantidadHabilidadesPersonaje :: Personaje -> Bool
cantidadHabilidadesPersonaje personaje = length (habilidades personaje) <=2

tiempo :: Gema
tiempo personaje = (quitarEnergia 50.reducirEdad) personaje
            --   = quitarEnergia 50 personaje{edad=(max 18. div (edad personaje)2}

reducirEdad ::  Personaje -> Personaje
reducirEdad personaje 
    | edad personaje `div` 2 < 18 = personaje{edad=18}
    | otherwise = personaje{edad=edad personaje `div` 2}

gemaLoca :: Gema -> Gema
gemaLoca gema personaje = (gema.gema) personaje

--PUNTO 4)
guantelete1 :: Guantalete
guantelete1 = Guantalete "Guante de Goma" [tiempo,alma "Mjolnir",gemaLoca (alma "programacion en Haskell")]

--PUNTO 5)
utilizar :: [Gema] -> Personaje -> Personaje  -- podes hacer Gema
utilizar listaGemas personaje = foldl (\personaje gema -> gema personaje) personaje listaGemas

--PUNTO 6)
gemaMasPoderosa :: Personaje -> Guantalete  -> Gema
gemaMasPoderosa personaje guantelete = gemaDeMayorPoder personaje (gemasPosee guantelete)

gemaDeMayorPoder :: Personaje -> [Gema] -> Gema
gemaDeMayorPoder _ [gema] = gema  
gemaDeMayorPoder personaje (gema1:gema2:gemas) 
    | (energia.gema1) personaje > (energia.gema2) personaje = gemaDeMayorPoder personaje (gema2:gemas)
    | otherwise = gemaDeMayorPoder personaje (gema1:gemas) 

--PUNTO 7)
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantalete
guanteleteDeLocos = Guantalete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantalete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantalete = (utilizar . take 3. gemasPosee) guantalete






