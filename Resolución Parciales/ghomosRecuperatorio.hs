module Library where
import PdePreludat

data Material = Material{
    nombre :: String,
    calidad :: Number
}deriving(Show,Eq)

data Edificio = Edificio{
    tipoEdificio :: String,
    materiales :: [Material]
}deriving(Show,Eq)

data Aldea = Aldea{
    poblacion :: Number,
    materialesDisponibles :: [Material],
    edificios :: [Edificio]
}deriving(Show,Eq)

-- PUNTO 1: Desarrollar las siguientes funciones.
--a) esValioso que recibe un material y retorna True si su calidad es mayor a 20. 
-- Ej: > esValioso (Material "Madera de Pino" 25)

esValioso :: Material -> Bool 
esValioso = (>20).calidad

--b) unidadesDisponibles que recibe el nombre de un material y una aldea y retona la cantidad
-- de materiales disponibles con ese nombre en la aldea. 
-- Ej: > unidadesDisponibles "Acero" (Aldea 50 [(Material "Acero" 15), (Material "Acero" 20), (Material "Piedra" 5)] [])

unidadesDisponibles :: String -> Aldea -> Number
unidadesDisponibles nomMat = length . filter (==nomMat) . map nombre . materialesDisponibles


--c) valorTotal que recibe una aldea y retorna la suma de la calidad de todos los materiales
-- que hay en la aldea. Estos son tanto los disponibles como los usados en sus edificios. 
-- Ej: > valorTotal (Aldea 50 [(Material "Acero" 15), (Material "Piedra" 5)] [(Edificio "Barracas" [(Material "Acero" 20)] [])

valorTotal :: Aldea -> Number 
valorTotal aldea = calidadMateriales (materialesDisponibles aldea) + calidadMaterialesEdificio (edificios aldea)

calidadMateriales :: [Material] -> Number 
calidadMateriales =  sum . map calidad

calidadMaterialesEdificio :: [Edificio] -> Number
calidadMaterialesEdificio  =  sum . map (calidadMateriales . materiales) 

-- PUNTO 2: Desarrollar las sig tareas para que los ghomos puedan realizar en una aldea. 
--a) tenerGnomito que aumenta la poblacion de la aldea en 1.

tenerGnomito :: Tarea
tenerGnomito aldea = aldea{poblacion=poblacion aldea + 1}

--b) lustrarMaderas que aumenta en 5 la calidad de todos los materiales disponibles cuyo nombre empiece con la 
-- palabra "Madera". El resto de los materiales de la aldea no deberian verse afectados al realizar esta tarea. 

lustrarMaderas :: Tarea
lustrarMaderas aldea = aldea{materialesDisponibles= (map subirCalidadMateriales . materialesDisponibles) aldea }

subirCalidadMateriales :: Material -> Material
subirCalidadMateriales material
    | ((=="Madera").nombre) material = material{calidad=calidad material + 5}
    | otherwise = material

--c) recolectar que dado un material y una cantidad de cuanto de se material se quiere recolectar,
-- incorpore a los materiales disponibles de la aldea ese mismo material tantas veces como se indique. 

recolectar :: Material -> Number -> Tarea
recolectar material cantidad aldea = aldea{materialesDisponibles = replicate cantidad material ++ materialesDisponibles aldea}

-- PUNTO 3: Realizar las consultas que permitan
--a) Obtener los edificios chetos de una aldea, que son aquellos que tienen algún material valioso. 

-- > (filter (any esValioso . materiales) . edificios) aldea

--b) Obtener una lista de nombres de materiales comunes, que son aquellos que se encuentran en todos los edificios de la aldea. 

-- >  (map nombre . filter (all (==material)) . materiales) (edificios aldea) 

-- PUNTO 4:
--a) Definir la funcion realizarLasQueCUmplen :: [Tarea] -> (Aldea -> Bool) -> Aldea -> Aldea  
-- que recibe una lista de tareas, un criterio que deberia cumplir la aldea luego de realizar cada tarea
-- y la aldea iniciail, y retorne cómo quedaría la aldea si se realizaran las tareas válidas, una tras otra. Una tarea es válida 
-- si, después de realizarse sobre una aldea (la original o la resultante de haber realizado otra tarea previa), 
-- la misma cumple con el criterio indicado. 

type Tarea = Aldea -> Aldea

realizarLasQueCumplen :: [Tarea] -> (Aldea -> Bool) -> Aldea -> Aldea 
realizarLasQueCumplen tareas condicion aldea = foldl realizarTarea aldea (tareasValidas condicion aldea tareas)

realizarTarea :: Aldea -> Tarea -> Aldea 
realizarTarea aldea tarea = tarea aldea

esTareaValida  :: (Aldea -> Bool) -> Aldea -> Tarea-> Bool 
esTareaValida condicion aldea tarea =  condicion (tarea aldea)

tareasValidas :: (Aldea -> Bool) -> Aldea -> [Tarea] -> [Tarea]
tareasValidas condicion aldea = filter (esTareaValida condicion aldea)





