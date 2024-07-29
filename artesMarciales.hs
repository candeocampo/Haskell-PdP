module Library where
import PdePreludat


data Peleador = Peleador{
    nombre :: String,
    puntosDeVida :: Number,
    resistencia :: Number,
    ataques :: [Ataque]
}deriving(Show,Eq)

type Ataque = Peleador -> Peleador

-- Punto 1 --
estaMuerto :: Peleador -> Bool
estaMuerto = (==0).puntosDeVida

esHabil :: Peleador -> Bool
esHabil peleador = ataquesQueConoce (ataques peleador) && ((>=15).resistencia) peleador

ataquesQueConoce :: [Ataque] -> Bool
ataquesQueConoce = (<=10) . length

perderVida :: Number -> Peleador -> Peleador
perderVida cantidad peleador = peleador{puntosDeVida = max 0 . subtract cantidad . puntosDeVida $ peleador}

-- Punto 2 --
type Intensidad = Number

golpe :: Intensidad -> Ataque
golpe intensidad peleador = peleador{puntosDeVida = intensidad / resistencia peleador}

toqueDeLaMuerte :: Ataque
toqueDeLaMuerte peleador = peleador{puntosDeVida=0}

patada :: String -> Ataque
patada tipoPatada = modificarResistencia . patadaEn tipoPatada

modificarResistencia :: Peleador -> Peleador
modificarResistencia oponente = oponente{resistencia = (subtract 1 . resistencia) oponente}

olvidarPrimerAtaque :: Peleador -> Peleador
olvidarPrimerAtaque oponente = oponente{ataques=(tail.ataques) oponente}

patadaEn :: String -> Ataque
patadaEn tipoPatada oponente
    | tipoPatada == "pecho" && (not.estaMuerto) oponente = perderVida 10 oponente
    | tipoPatada == "pecho" && estaMuerto oponente = perderVida (-1) oponente
    | tipoPatada == "carita" = perderVida (((/2).puntosDeVida) oponente) oponente
    | tipoPatada == "nunca" = olvidarPrimerAtaque oponente
    | otherwise = oponente

tripleAtaque :: Ataque -> Ataque
tripleAtaque ataque = ataque . ataque . ataque

-- Punto 3 --
bruceLee :: Peleador
bruceLee = Peleador "Bruce Lee" 200 25 [toqueDeLaMuerte, golpe 500, patada "nuca", tripleAtaque (patada "carita"), ataqueEspecial]

ataqueEspecial :: Ataque
ataqueEspecial oponente = oponente{ataques = []}

-- Punto 4 --
mejorSegun :: Ord a => (b->a) -> c -> [c->b] -> (c->b)
mejorSegun _ _ [x] = x
mejorSegun criterio p (x:y:xs)
    | (criterio.x) p < (criterio.y) p = mejorSegun criterio p (x:xs)
    | otherwise = mejorSegun criterio p (y:xs)

mejorAtaquePeleador :: Peleador -> Peleador -> Ataque
mejorAtaquePeleador peleador enemigo = mejorSegun puntosDeVida enemigo (ataques peleador)

-- Punto 5 --
terribles :: Peleador -> [Peleador] -> [Ataque]
terribles peleador enemigos = undefined

esTerrible :: Ataque -> [Peleador] -> Bool
esTerrible ataque enemigos = ((`div`2) . length) enemigos > (vivos . enemigosRecibenAtaque ataque) enemigos 

enemigosRecibenAtaque :: Ataque -> [Peleador] -> [Peleador]
enemigosRecibenAtaque ataque = map ataque 

vivos :: [Peleador] -> Number
vivos = length . filter (not.estaMuerto)

-- 5.b 
peligroso :: Peleador -> [Peleador] -> Bool
peligroso peleador enemigos = esHabil peleador && all (flip algunAtaqueMortal enemigos) (ataques peleador)

algunAtaqueMortal:: Ataque -> [Peleador] -> Bool
algunAtaqueMortal ataque = any (ataqueMortal ataque)
-- se podia hacer un map ataque y después el si alguno está muerto. 

ataqueMortal :: Ataque -> Peleador -> Bool
ataqueMortal ataquePeleador = estaMuerto . recibirAtaque ataquePeleador

recibirAtaque :: Ataque -> Peleador -> Peleador 
recibirAtaque ataquePeleador = ataquePeleador 

{--
--Si es mortal, el ataque mata al enemigo
esMortal :: Peleador -> Ataque -> Bool
esMortal enemigo ataque = estaMuerto (ataque enemigo)

--Todos los ataques son mortales sobre un Enemigo
sonTodosMortales :: Peleador -> [Ataque] -> Bool
sonTodosMortales enemigo = all (esMortal enemigo)

--En un grupo de enemigos, alguno cumple sonTodosMortales
algunoTodosMortales :: [Ataque] -> [Peleador] -> Bool
algunoTodosMortales ataques = any (flip sonTodosMortales ataques) 

--El peleador es habil y sus ataques son todos mortales para algun enemigo
esPeligroso peleador enemigos = esHabil peleador && algunoTodosMortales (ataques peleador) enemigos
--}

-- 5.c 
invencible :: Peleador -> [Peleador] -> Bool
invencible peleador enemigos = puntosDeVida peleador == (puntosDeVida .recibirAtaquesEnemigos peleador) enemigos

primeros10AtaquesEnemigos :: [Peleador] -> [Ataque]
primeros10AtaquesEnemigos = take 10 . concat . map ataques . filter esHabil

recibirAtaquesEnemigos :: Peleador -> [Peleador] -> Peleador
recibirAtaquesEnemigos peleador = foldl (flip($)) peleador . primeros10AtaquesEnemigos









