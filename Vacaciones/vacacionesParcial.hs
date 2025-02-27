module Library where
import PdePreludat

data Turista = Turista{
    cansancio :: Number,
    stress :: Number,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving(Show,Eq)

type Idioma = String
type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista  
    | ((==True).viajaSolo) turista = modificarCansancio 5 turista
    | otherwise = modificarStress 1 turista

modificarCansancio :: Number -> Turista -> Turista
modificarCansancio valor turista = turista{cansancio = cansancio turista - valor}

modificarStress :: Number -> Turista -> Turista
modificarStress valor turista = turista{stress = stress turista - valor}

apreciarPaisaje :: String -> Excursion
apreciarPaisaje elemento = modificarStress (length elemento)

salirAHablarIdioma :: String -> Excursion
salirAHablarIdioma idiomaNuevo = viajarAcompaniado .incorporarNuevoIdioma idiomaNuevo

incorporarNuevoIdioma :: String -> Turista -> Turista
incorporarNuevoIdioma idioma turista = turista{idiomas= idioma : idiomas turista}

viajarAcompaniado :: Excursion
viajarAcompaniado turista = turista{viajaSolo=False}

caminar :: Number -> Excursion
caminar minutos  = modificarCansancio (-(minutos`div`4)) . modificarStress (minutos`div`4)

type Marea = String
paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea turista 
    | (== "fuerte") marea = (modificarStress (-6) . modificarCansancio (-10)) turista
    | (== "tranquila") marea = (caminar 10 . apreciarPaisaje "mar" . salirAHablarIdioma "aleman") turista
    | (== "moderada") marea = turista

-- punto 1 --
ana :: Turista
ana = Turista 0 21 False ["espaniol"]

beto :: Turista
beto = Turista 15 15 True ["aleman"]

cathi :: Turista
cathi = Turista 15 15 True ["aleman","catalan"]

-- punto 2 -- 
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = modificarStressPorcentaje 10 . excursion

modificarStressPorcentaje :: Number -> Turista -> Turista
modificarStressPorcentaje porcentaje turista = turista{stress = stress turista - (porcentaje * stress turista)/100}

-- 2.b -- 
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice turista (excursion turista)

-- punto c --
excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa turista  = (>0) . deltaExcursionSegun (length . idiomas) turista

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista  = filter (reduceStress turista)

reduceStress :: Turista -> Excursion -> Bool
reduceStress turista =  (<= -3) . deltaExcursionSegun stress turista 

-- punto 3 --
type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarPaisaje "cascada", caminar 40, salirAHablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB elegirExcursion = [paseoEnBarco "tranquila", elegirExcursion, caminar 120 ]

islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnBarco marea, excursionEnIslaVecina marea, paseoEnBarco marea]

excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina marea 
    | (== "fuerte") marea = apreciarPaisaje "lago"
    | otherwise = irALaPlaya

-- 3.a --
hacerTour :: Tour -> Turista -> Turista
hacerTour tour  = flip realizarExcursiones tour . modificarStress (-(length tour)) 

realizarExcursiones :: Turista -> Tour -> Turista
realizarExcursiones turista = foldl (flip hacerExcursion) turista

-- 3.b --
convicente :: Turista -> [Tour] -> Bool
convicente turista = any (esConvicente turista)

esConvicente :: Turista -> Tour -> Bool
esConvicente turista  = any (dejaAcompaniado turista) . excursionesDesestresantes turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista  = not . viajaSolo . flip hacerExcursion turista

