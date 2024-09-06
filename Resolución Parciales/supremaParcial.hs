module Library where
import PdePreludat

data Ley = Ley{
    tema :: String,
    presupuesto :: Number,
    partidoPolitico :: [String]
} deriving(Show,Eq)

leyCannabis :: Ley
leyCannabis = Ley "Ley de uso medicinal del cannabis" 5 ["cambio de todos", "sector financiero"]

leyEducacionSuperior :: Ley
leyEducacionSuperior = Ley "Ley de educacion superior" 30 ["docentes universitarios", "partido de centro federal"]

leyDeProfesionalizacon :: Ley
leyDeProfesionalizacon = Ley "Ley de profesionalizacion del tenista de mesa" 1 ["partido de centro federal","liga de deportistas autonomos","club paleta veloz"]

leyTenis :: Ley
leyTenis = Ley "Ley de Tenis" 2 ["liga de deportistas autonomos"]

-- punto 1 --
leyesCompatibles :: Ley -> Ley -> Bool
leyesCompatibles ley1 ley2 = temaIncluido ley1 ley2  && tienenSectorEnComun (partidoPolitico ley1) (partidoPolitico ley2)

temaIncluido ::  Ley -> Ley -> Bool
temaIncluido ley1 ley2 = ((== "Ley de Tenis").tema) ley1 && ((=="Ley de profesionalizacion del tenis de mesa").tema) ley2

tienenSectorEnComun :: [String] -> [String] -> Bool
tienenSectorEnComun [] _ = False
tienenSectorEnComun (x:xs) ys
  | x `elem` ys = True
  | otherwise   = tienenSectorEnComun xs ys

-- parte B --
data Juez = Juez {
    criterioDeVotacion :: Ley -> Bool
} deriving (Show, Eq)

type Agenda = [Ley]

criterioOpinionPublica :: Agenda -> Ley -> Bool
criterioOpinionPublica agenda ley = elem ley agenda

apoyoSectorFinanciero :: String -> Ley -> Bool
apoyoSectorFinanciero partido = elem "sector financiero". partidoPolitico

inconstitucionalSegunPresupuesto :: Number -> Ley -> Bool
inconstitucionalSegunPresupuesto n = (>n).presupuesto

criterioConservador :: Ley -> Bool
criterioConservador  = apoyoSectorFinanciero "partido conservador" 

-- punto 1 --
type CorteSuprema = [Juez]

esConstitucional :: Ley -> Juez -> Bool
esConstitucional ley juez = criterioDeVotacion juez ley

-- punto 2 --
votoAfirmativo :: Ley-> Bool
votoAfirmativo ley = True

juezAfirmativo :: Juez
juezAfirmativo = Juez votoAfirmativo

-- juez que vote si tiene apoyo de más de 2 partidos politicos.
apoyoPartidos :: Ley -> Bool
apoyoPartidos = (>2).length.partidoPolitico

juezInventado :: Juez
juezInventado = Juez apoyoPartidos

juezPreocupacion :: Juez
juezPreocupacion = Juez (inconstitucionalSegunPresupuesto 1000)




