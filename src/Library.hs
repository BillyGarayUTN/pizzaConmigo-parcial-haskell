module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero * 2

data Pizza = Pizza{
    ingredientes::[String],
    tamanio::Number,
    calorias::Number
} deriving(Show,Eq)

grandeDeMuzza = Pizza{ingredientes=["salsa","mozzarella","oregano"], tamanio=8,calorias=350}

nivelDeSatisfaccion :: Pizza -> Number
nivelDeSatisfaccion pizza
    | "palmito" `elem` ingredientes pizza = 0
    | (500<).calorias $ pizza = length.ingredientes $ pizza
    | otherwise = (`div` 2).length.ingredientes $ pizza

valorPizza :: Pizza -> Number
valorPizza pizza  = (tamanio pizza *).(120*).length.ingredientes $ pizza

nuevoIngrediente :: String -> Pizza -> Pizza
nuevoIngrediente ingredienteNuevo pizza = pizza { ingredientes = (ingredienteNuevo:).ingredientes $ pizza,
    calorias = (length ingredienteNuevo +).calorias $ pizza }

agrandar :: Pizza -> Pizza
agrandar pizza = pizza{ tamanio = min 10.(2+).tamanio $ pizza}

mezcladita ::  Pizza -> Pizza -> Pizza
mezcladita primerPizza pizza = pizza{ ,calorias = (calorias pizza+).(`div` 2) $ calorias primerPizza }

--aux ingredienteRepetido
ingredienteRepetido :: String->Pizza -> Pizza
ingredienteRepetido ingrediente pizza
    | elem ingrediente $ ingredientes pizza = pizza
    | otherwise = ingredientes = pizza { ingredientes = (ingredienteNuevo:).ingredientes $ pizza}
