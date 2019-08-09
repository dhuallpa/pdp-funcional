import Data.List
import Text.Show.Functions
type Gema = Personaje->Personaje
data Guantelete = Guantelete{
 material::String,
 gemas::[Gema]
} deriving (Show) 

data Personaje = Personaje{
 edad::Int,
 energia::Int,
 habilidades::[String],
 nombre::String,
 planeta::String
} deriving (Show)

type Universo = [Personaje]

jack = Personaje { edad=33,energia=3,habilidades=["volar","usar Mjolnir"],nombre="jack",planeta="asgard"}
hulk = Personaje { edad=47,energia=12,habilidades=["superfuerza"],nombre="hulk",planeta="tierra"}
ironMan = Personaje { edad=45,energia=8,habilidades=["volar","traje","rayos"],nombre="ironMan",planeta="tierra"}


marvel :: Universo
marvel = [jack,hulk,ironMan]


guantelete1 = Guantelete {material="uru", gemas=[gemaMente 4,gemaAlma "volar",gemaEspacio "yorda",gemaLoca (gemaAlma "rod  q"),gemaPoder,gemaTiempo]}
guantelete2 = Guantelete {material="goma", gemas=[gemaAlma "usar Mjolnir",gemaTiempo,gemaLoca (gemaAlma "programación en Haskell")]}

guanteCompleto guantelete = material guantelete =="uru" && length (gemas guantelete) == 6

chasquidoUniverso guantelete universo 
 |guanteCompleto guantelete = take ( div (length universo) 2) universo
 |otherwise = universo
 

--aptoPendex universo = any ((<45).edad)(universo)
aptoPendex universo = any (<45)(edades universo)
edades universo = map (edad) universo

energiaTotal universo =  sum (total universo)
total universo = map energia (filter ((>1).length.habilidades) universo)

gemaMente valor personaje  = personaje {energia = energia personaje - valor }

gemaAlma habilidad personaje  = personaje {habilidades = filter (/=habilidad) (habilidades personaje) ,energia = energia personaje - 10 }

gemaEspacio planet personaje  = personaje {planeta = planet ,energia = energia personaje -20 }

gemaPoder personaje = personaje {energia = 0,habilidades = menosDeDos personaje }

menosDeDos personaje 
 | (length.habilidades) personaje <=2  = []
 |otherwise = habilidades personaje

gemaTiempo personaje = personaje { edad = reducir personaje, energia = energia personaje -50} 

reducir personaje 
 | div (edad personaje) 2 >= 18 =  div (edad personaje) 2
 |otherwise = 18
 
gemaLoca gema personaje = gema (gema  personaje)

uso :: Gema->Personaje->Personaje
uso gema personaje = gema personaje
utilizar gemas personaje = foldr uso personaje gemas

gemaMasPoderosa (Guantelete {material=e,gemas=[x]}) personaje = x

gemaMasPoderosa (Guantelete {material=e ,gemas=(x:y:ys)}) personaje 
 | energia (x personaje) <=energia (y personaje) = gemaMasPoderosa (Guantelete {material=e ,gemas=(x:ys)}) personaje
 |otherwise = gemaMasPoderosa (Guantelete {material=e ,gemas=(y:ys)}) personaje
 
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas gemaTiempo)


usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete
