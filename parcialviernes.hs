import Data.List
import Text.Show.Functions
type Seguridad = (String,[Int])
 
data Obrero = Obrero {
 tareas :: [String],
 instrumental :: Seguridad,
 sueldo :: Float,
 blanco :: Bool
 } deriving (Eq,Show)
 
primElemento =fst.instrumental 

mejoraSeguridad obrero 
 | ((== "ninguno").primElemento) obrero  = obrero {instrumental = mejoraNinguno (instrumental obrero) }
 | ((== "basica" ).primElemento) obrero = obrero {instrumental = mejoraBasica (instrumental obrero) }
 |otherwise = obrero {instrumental = mejoraReforzada (instrumental obrero) }
 
 
mejoraNinguno instrumental = ("basica",[10])


mejoraBasica instrumental 
 | (head.snd) instrumental <10= ("basica",(map (*2) (snd instrumental)))
 | otherwise = ("reforzada",(snd instrumental) ++ [0])

mejoraReforzada instrumental = ("reforzada",[(head.snd) instrumental,(last.snd) instrumental + (head.snd) instrumental])

hugo :: Obrero
hugo = Obrero {tareas = ["soldar","armar"], instrumental = ("ninguno",[]), sueldo= 11000, blanco=False} 
juan :: Obrero
juan = Obrero {tareas = ["vigilante","hacer mandados","enviar"], instrumental = ("basica",[12]), sueldo= 12000, blanco=False} 
lucas :: Obrero
lucas = Obrero {tareas = ["fundir"], instrumental = ("reforzada",[12,5]), sueldo= 15000, blanco=True} 
anastasio:: Obrero
anastasio = Obrero {tareas = ["limpiar"], instrumental = ("ninguno",[]), sueldo= 10000, blanco=False} 

preocupan = ["pulir","soldar","fundir"]

imprescindibles = ["soldar","fundir","armar","controlar"]
hayInterseccion lista= noVacio intersect lista 
noVacio funcion lista= (not.null).funcion lista

gerenteGenerico obrero mejora lista
 |hayInterseccion lista (tareas obrero) = mejora obrero
 |otherwise = obrero

gerenteSeguridad obrero = gerenteGenerico obrero mejoraSeguridad preocupan
gerenteAdministrativo obrero = gerenteGenerico obrero mejoraSueldo ["hacer mandados"]
gerentePersonal obrero = gerenteGenerico obrero ponerEnBlanco imprescindibles

 
mejoraSueldo obrero  = obrero {sueldo = (sueldo obrero*1.15)} 

ponerEnBlanco obrero = obrero {blanco=True} 

reclamoIndividual  obrero gerente= gerente obrero

obreros1 :: [Obrero]
obreros1 = [juan,hugo,lucas]
reunion obreros gerente = map (gerente.gerente) obreros


estaEnBlanco obreros = filter (blanco) obreros 

tomaDeLaFabrica [] gerentes = []  


tomaDeLaFabrica obreros  gerentes =  (reclamoColectivo (head obreros) gerentes)++ tomaDeLaFabrica (tail obreros) gerentes
reclamoColectivo obrero [] = obrero
reclamoColectivo obrero gerentes= reclamoColectivo (reclamoIndividual  obrero (head gerentes)) (tail gerentes)
tomaDeLaFabrica obrero gerentes = obrero 
mejoraExtra obrero = obrero {tareas = (tareas obrero)++["Cerrar Oficina"],sueldo=(sueldo obrero)*1.1 }  
gerenteTareas obrero = gerenteGenerico obrero mejoraExtra ["limpiar"]

--En tomaDeLaFabrica si la lista de obreros es infinita la funcion entra en un loop infinito por el filter dado que filtrara 
--a los obreros que esten en blanco en esta lista infinita
-- si fueran gerentes infinitos pasaria lo mismo dado que nunca se dejaria de entrar en un gerente (funcion)
-- esto se le llama eager evaluation 
--ya que no puedo tomar lo necesario lazy evaluation

--un caso es en novacio donde se uso composicion y aplicacion parcial para que se entienda mas ya que sino estaria lleno de ()
-- una funcion tras otra f(g(r())) y lo haria poco entendible 