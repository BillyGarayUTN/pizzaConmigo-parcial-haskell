module Library where
import PdePreludat

funcionLoca :: Number -> String -> Number 
funcionLoca numero palabra
    | odd numero                     = numero
    | (length palabra) > numero      = length palabra
    | otherwise                      = numero `mod` (length palabra)

doble :: Number -> Number
doble numero = numero * 2

siguiente :: Number  -> Number
siguiente numero = numero + 1

siguienteEsPar :: Number -> Bool
siguienteEsPar numero= (even . siguiente) numero

type Nombre = String
type Edad = Number
type Persona = (Nombre,Edad)
clara :: Persona
clara = ("clara",27)

obtenerEdad :: Persona -> Edad
obtenerEdad = snd 