module Library where
import PdePreludat

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Number,
  superficie :: Number,
  precio :: Number,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}deriving (Show, Eq)

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]

-- 1)

--a)
mayor :: Ord a => (t -> a) -> t -> t -> Bool
mayor funcion valor1 =  (< funcion valor1).(funcion)

menor :: Ord a => (t -> a) -> t -> t -> Bool
menor funcion valor1 = (> funcion valor1).(funcion)

--b)
listaStringsEj = ["Hola","Buena","Noches"]

ordenarSegunLargo :: [String] -> [String]
ordenarSegunLargo = ordenarSegun (mayor length)

-- 2)

--a)
deptoEj = Depto 4 80 7500 "Palermo"

listaBarriosEj = ["Palermo","Liniers","Villa Urquiza"]

ubicadoEn :: [Barrio] -> Requisito
ubicadoEn listaBarrios depto = any (== barrio depto) listaBarrios

--b)
cumpleRango :: (Depto -> Number) -> Number -> Number -> Requisito
cumpleRango criterio n1 n2 depto = between n1 n2 (criterio depto)

-- 3)

--a)
cumpleBusqueda :: Busqueda -> Depto -> Bool
cumpleBusqueda busqueda depto = all (==True) (aplicarRequisitosADepto depto busqueda)

aplicarRequisitosADepto :: Depto -> [Depto -> a] -> [a]
aplicarRequisitosADepto depto requisitos = foldr (aplicarUnRequisitoADepto depto) [] requisitos

aplicarUnRequisitoADepto :: Depto -> (Depto -> a) -> [a] -> [a]
aplicarUnRequisitoADepto depto requisito1 requisitos = requisito1 depto : requisitos

--b) 
buscar :: [Depto] -> (Depto -> Requisito) -> Busqueda -> [Depto]
buscar deptos criterioOrd busqueda = ordenarSegun criterioOrd (deptosQueCumplenBusqueda deptos busqueda)

deptosQueCumplenBusqueda :: [Depto] -> Busqueda -> [Depto]
deptosQueCumplenBusqueda deptos busqueda = filter (cumpleBusqueda busqueda) deptos

--c) buscar busquedaEj (mayor superficie) deptosDeEjemplo

busquedaEj = [ubicadoEn ["Palermo","Recoleta"],cumpleRango ambientes 0 2, cumpleRango precio 0 6000]
busquedaEj2 = [ubicadoEn ["Villa Urquiza"],cumpleRango ambientes 0 5, cumpleRango precio 0 4000]
-- 4)

augusto = Persona {
    mail = "augustocrack@gmail.com",
    busquedas = [busquedaEj]
}

lean = Persona {
    mail = "leancrack@gmail.com",
    busquedas = [busquedaEj,busquedaEj2]
}

deptoEj2 = Depto 3 20 3500 "Villa Urquiza"

mailsDePersonasInteresadas :: Depto -> [Persona] -> [String]
mailsDePersonasInteresadas depto = map (mailDePersonaInteresada depto)

mailDePersonaInteresada :: Depto -> Persona -> String
mailDePersonaInteresada depto persona
    | all (==[]) (cumpleAlgunaBusqueda depto persona) = "No esta interesada"
    | otherwise = mail persona

cumpleAlgunaBusqueda :: Depto -> Persona -> [[Depto]]
cumpleAlgunaBusqueda depto persona = map (deptosQueCumplenBusqueda [depto]) (busquedas persona)











