import Text.Show.Functions()
data Persona=Persona {
    edad::Int,
    felicidonios :: Int,
    suenios::[(Persona->Persona)],
    nombre::String,
    habilidades::[String]
} deriving Show

recibirse::String->Persona->Persona
recibirse carrera persona=persona{felicidonios=(felicidonios persona)+1000*(length carrera),habilidades=(habilidades persona) ++ [carrera]}

viajar :: [String]-> Persona -> Persona
viajar ciudad persona=persona{edad=(edad persona)+1, felicidonios=(felicidonios persona)+ 100*(length ciudad)}

enamorarse::Persona->Persona->Persona
enamorarse persona2 persona1= persona1{felicidonios=(felicidonios persona1) + (felicidonios persona2)}

laura :: Persona
laura=Persona{
    edad=21,
    felicidonios=0,
    suenios=[recibirse "Arquitectura", viajar ["Paris","Chascomus","sdfdssjfsjdfsgdgfajdjfajs","sdgfhskgfdhhsdfghds"],enamorarse roberto],
    nombre="Laura Hernandez",
    habilidades=[]
}

roberto::Persona
roberto=Persona{
    edad=25,
    felicidonios=53,
    suenios=[recibirse "Medico", viajar ["Cordoba"],enamorarse laura],
    nombre="Roberto Perez",
    habilidades=["Saltar alto"]
}

 
coeficienteSatisfaccion::Persona -> Int
coeficienteSatisfaccion persona 
    | (>100) (felicidonios persona)=(felicidonios persona)*(edad persona)
    | (>50) (felicidonios persona) && (<=100) (felicidonios persona)=(length(suenios persona))*(felicidonios persona)
    | otherwise=div (felicidonios persona) 2

gradoAmbicion:: Persona -> Int
gradoAmbicion persona
    | (>100) (felicidonios persona) =felicidonios persona * length (suenios persona)
    | (>50) (felicidonios persona) && (<=100) (felicidonios persona)= edad persona * length (suenios persona)
    | otherwise= length(suenios persona)*2

nombreLargo::Persona->Bool
nombreLargo persona=((>10).length) (nombre persona)
 
suertuda::Persona->Bool
suertuda=even.(*3).coeficienteSatisfaccion
 
comboPerfecto::Persona->Persona
comboPerfecto persona = ((viajar ["Berazategui", "Paris"]).(recibirse "Medicina")) persona{felicidonios=(felicidonios persona)+100}

quitarSuenio::Persona->Int->Persona
quitarSuenio persona n=persona{suenios=drop n (suenios persona)}

fuenteMinimalista::Persona->Persona
fuenteMinimalista persona=quitarSuenio ((head(suenios persona)) persona) 1

aplicaFunciones::Persona->[(Persona->Persona)]->Persona
aplicaFunciones persona (x:xs)
    |length(xs)/=0=aplicaFunciones (x persona) xs
    |otherwise=x persona

queTodoSigaIgualConformista  = id

fuenteCopada::Persona->Persona
fuenteCopada persona= quitarSuenio ((aplicaFunciones persona)(suenios persona)) (length(suenios persona))

fuenteAPedido::Int->Persona->Persona
fuenteAPedido num persona
    |length(suenios persona)==0=persona
    |otherwise=(head(drop (num-1) (suenios persona))) persona

fuenteSorda = id


maximoFelicidonios::[(Persona->Persona)]->Persona->(Persona->Persona)
maximoFelicidonios fuentes persona=head(filter ((==maximo).(felicidonios).($ persona)) fuentes)
    where maximo=maximum $ map ((felicidonios).($ persona)) fuentes

minimoFelicidonios::[(Persona->Persona)]->Persona->(Persona->Persona)
minimoFelicidonios fuentes persona=head(filter ((==minimo).(felicidonios).($ persona)) fuentes)
    where minimo=minimum $ map ((felicidonios).($ persona)) fuentes

maximoHabilidades::[(Persona->Persona)]->Persona->(Persona->Persona)
maximoHabilidades fuentes persona=head(filter ((==maximo).(felicidonios).($ persona)) fuentes)
    where maximo=maximum $ map ((length).(habilidades).($ persona)) fuentes

fuenteGanadora::[(Persona->Persona)]->Persona->([(Persona->Persona)]->Persona->(Persona->Persona))->(Persona->Persona)
fuenteGanadora fuentes persona criterio=criterio fuentes persona

--suenioValioso::Persona->[(Persona->Persona)]
restafelicidonios::Persona->[(Persona->Persona)]
restafelicidonios persona=filter ((>100).(felicidonios).($ persona)) (suenios persona)
--suenioValioso persona= (filter ((>100).valorFinal.(felicidonios).($ persona))).(suenios)

--suenioRaro persona=any ((==(felicidonios persona)).(map ($ persona))) (suenios persona)

felicidadTotal=sum.(map ((felicidonios).fuenteCopada))
--Punto 7:
jorge=Persona 43 345 [recibirse ['a'..]] "Jorge Alvarez" ["Carpinteria"]
--Al usar a jorge en las distintas fuentes, con la excepción de la que funciona a pedido (mientras que el numero pedido sea >1), las demás, funcionan pero no convergen a ningún valor
--específico, por lo que podemos decir que divergen.