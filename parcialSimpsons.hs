data Personaje = Personaje{
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
}deriving(Show,Eq)


aumentarFelicidad :: Int -> Personaje -> Personaje
aumentarFelicidad valor personaje = Personaje{felicidad= nivelFelicidad (felicidad personaje) valor}


nivelFelicidad :: Int -> Int -> Int
nivelFelicidad actualNivel variacion
    | actualNivel + variacion > 0 = actualNivel + variacion
    | otherwise = 0

actualizarDinero :: Int -> Personaje -> Personaje
actualizarDinero valor personaje = Personaje{dinero=dinero personaje + valor}

irEscuela :: Personaje -> Personaje
irEscuela personaje 
    |  not(quienEs "Lisa" personaje)= aumentarFelicidad (-20) personaje
    | otherwise = aumentarFelicidad 20 personaje

quienEs :: String -> Personaje -> Bool
quienEs nombrePersonaje personaje = nombre personaje /= nombrePersonaje

comerDonas ::  Personaje -> Personaje
comerDonas = aumentarFelicidad 10.actualizarDinero(-10)

cantidadDonas :: Int -> Personaje -> Personaje
cantidadDonas cantidad personaje 
    | cantidad>=0 = cantidadDonas (cantidad-1)(comerDonas personaje)
    | otherwise = personaje

irTrabajar :: String -> Personaje -> Personaje
irTrabajar escuela personaje = actualizarDinero (trabajoCual escuela) personaje

trabajoCual :: String -> Int
trabajoCual trabajo = length trabajo

serDirector ::  Personaje -> Personaje
serDirector = irEscuela.irTrabajar "escuela elemental" 

homero, skinner, lisa :: Personaje

homero = Personaje "Homero Simpson" 50 100
skinner = Personaje "Skinner" 10 500
lisa = Personaje "Lisa Simpson" 100 0

srBurns :: Personaje
srBurns = Personaje "Sr.Burns" 0 1000000000

type Logro = Personaje -> Bool

serMillonario :: Logro 
serMillonario personaje = dinero personaje > dinero srBurns

alegrarseF :: Int -> Logro 
alegrarseF valor personaje = valor > felicidad personaje

programaKrosti :: Logro 
programaKrosti personaje = dinero personaje >= 10

type Actividad = Personaje -> Personaje
type Actividades = [Actividad]


{- unaActividadResultaDecisivaParaLograrUnLogro :: Actividades -> Logro -> Personaje -> Bool
unaActividadResultaDecisivaParaLograrUnLogro actividad logro personaje  
    | logro personaje = False
    | otherwise = (logro.actividades) personaje


actividadDecisiva :: Actividades -> Logro -> Personaje -> Personaje
actividadDecisiva [] logro personaje = personaje
actividadDecisiva (actividad:actividades) logro personaje  
    | unaActividadResultaDecisivaParaLograrUnLogro actividad logro personaje = actividad personaje 
    | otherwise = actividadDecisiva actividades logro personaje -}
