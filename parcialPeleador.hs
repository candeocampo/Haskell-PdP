module Lib where
import PdePreludat


data Peleador = Peleador{
    nombre :: String,
    vida :: Number,
    resistencia :: Number,
    ataques :: [Ataque]
}deriving(Show,Eq)

type Ataque = Peleador -> Peleador 

-- PUNTO 1
estaMuerto :: Peleador -> Bool 
estaMuerto = (==0).vida

esHabil :: Peleador -> Bool 
esHabil peleador = ((<=10).length.ataques) peleador && ((>=15).resistencia) peleador

perderVida :: Number -> Ataque 
perderVida valorVida peleador 
    | valorVida < vida peleador = peleador{vida= vida peleador - valorVida}
    | otherwise = peleador{vida=0}

-- PUNTO 2
type Intensidad = Number

golpe :: Intensidad -> Ataque
golpe intensidad enemigo = enemigo{vida=vida enemigo - (intensidad `div` resistencia enemigo)} 

toqueDeLaMuerte :: Ataque 
toqueDeLaMuerte enemigo = enemigo{vida=0}

type Patada = String -> Ataque 

ganaVida :: Number -> Ataque 
ganaVida valor peleador = peleador{vida=vida peleador+valor}

disminuirResistencia :: Number -> Ataque 
disminuirResistencia valorR enemigo = enemigo{resistencia=resistencia enemigo - valorR}

tipoDePatada :: Patada 
tipoDePatada nombre enemigo 
    | nombre == "pecho" && not(estaMuerto enemigo) = (disminuirResistencia 1 . perderVida 10) enemigo
    | nombre == "pecho" && estaMuerto enemigo = ( disminuirResistencia 1 . ganaVida 1) enemigo 
    | nombre == "carita" = disminuirResistencia 1 enemigo{vida= vida enemigo - (vida enemigo `div` 2 )}
    | nombre == "nuca" = disminuirResistencia 1 enemigo{ataques= (tail.ataques) enemigo}
    | otherwise = enemigo 

tripleAtaque :: Ataque -> Ataque 
tripleAtaque ataque = ataque . ataque . ataque

-- PUNTO 3
bruceLee :: Peleador
bruceLee = Peleador{
    nombre = "Bruce Lee",
    vida = 200,
    resistencia = 25,
    ataques = [toqueDeLaMuerte, golpe 500, tipoDePatada "nuca", tripleAtaque (tipoDePatada "carita")]
}

-- PUNTO 4

{--
f :: Ord a => ( b -> a) -> c -> [ c -> b ] -> ( c -> b)
f _ _ [x] = x 
f g p (x:y:xs) 
    | (g.x) p < (g.y) p = f g p (x:xs)
    | otherwise         = f g p (y:xs)
--}

--a) Explicar qué hace y reescribirla mejorándola en términos de expresividad. 

f :: (Ataque -> Number) -> Peleador -> [ Peleador -> Ataque ] -> ( Peleador -> Ataque)
f _ _ [x] = x 
f ponderacion peleador (ataque1:ataque2:ataques) 
    | (ponderacion.ataque1) peleador < (ponderacion.ataque2) peleador = f ponderacion peleador (ataque1:ataques)
    | otherwise         = f ponderacion peleador (ataque2:ataques)


--b) Queremos conocer cuál es el mejor ataque de un peleador contra un enemigo, siendo dicho ataque aquel que deje con menos vida al contrincante. 
mejorAtaque :: Peleador -> Peleador -> Ataque 
mejorAtaque peleador enemigo = f (flip vidaAnalizar enemigo) peleador (ataques peleador)

vidaAnalizar :: Ataque -> Peleador -> Number
vidaAnalizar ataque enemigo = vida (ataque enemigo)

-- PUNTO 5
--a)
ataquesTerribles :: Peleador -> [Peleador] -> [Ataque]
ataquesTerribles peleador enemigos = filter (flip  unAtaqueTerrible enemigos) (ataques peleador)

unAtaqueTerrible :: Ataque -> [Peleador] -> Bool 
unAtaqueTerrible ataque enemigos = ((`div`2) . length)enemigos < (length.filter(not.estaMuerto).map ataque) enemigos

--b)
peleadorPeligroso :: Peleador -> [Peleador] -> Bool 
peleadorPeligroso peleador enemigos = esHabil peleador && all (flip ataqueMortal enemigos) (ataques peleador)

ataqueMortal ::  Ataque -> [Peleador] -> Bool
ataqueMortal ataque =  any estaMuerto . map ataque

-- Si el peleador es peligroso para unos enemigos, que se cumple si es hábil y todos sus ataque son mortales para alguino de los enemigos.
-- El ataque se considera mortal cuando el enemigo está muerto luego de recibirlo.

































