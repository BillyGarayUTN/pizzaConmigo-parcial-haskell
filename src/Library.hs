module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero * 2


--punto 1
data Pizza = Pizza{
    ingredientes::[String],
    tamanio::Number,
    calorias::Number
} deriving(Show,Eq)

grandeDeMuzza = Pizza{ingredientes=["salsa","mozzarella","oregano"], tamanio=8,calorias=350}

--punto 2
nivelDeSatisfaccion :: Pizza -> Number
nivelDeSatisfaccion pizza
    | "palmito" `elem` ingredientes pizza = 0
    | (500<).calorias $ pizza = (80*).cantidadIngredientes $ pizza
    | otherwise = (`div` 2).(80*).cantidadIngredientes $ pizza

--Aux
cantidadIngredientes :: Pizza -> Number
cantidadIngredientes = length.ingredientes

-- punto 3
valorPizza :: Pizza -> Number
valorPizza pizza  = (tamanio pizza *).(120*).cantidadIngredientes $ pizza

--punto 4

nuevoIngrediente :: String -> Pizza -> Pizza
nuevoIngrediente ingredienteNuevo pizza = pizza { ingredientes = (ingredienteNuevo:).ingredientes $ pizza,
    calorias = (length ingredienteNuevo +).calorias $ pizza }

agrandar :: Pizza -> Pizza
agrandar pizza = pizza{ tamanio = min 10.(2+).tamanio $ pizza}


mezcladita ::  Pizza -> Pizza -> Pizza
mezcladita primerPizza pizza = (agregarCalorias primerPizza) $ foldr (ingredienteSinRepetir) pizza (ingredientes primerPizza)

-- filtrar sin repetir 
ingredienteSinRepetir :: String->Pizza -> Pizza
ingredienteSinRepetir ingredienteN pizza 
    | ingredienteN `elem` ingredientes pizza = pizza
    | otherwise = pizza{ ingredientes = (ingredienteN:).ingredientes $ pizza} 

agregarCalorias:: Pizza -> Pizza -> Pizza
agregarCalorias primerPizza pizza= pizza { calorias = (calorias pizza+).(`div` 2).calorias $ primerPizza}



mezcladita' primeraPizza segundaPizza = 
    segundaPizza {
        ingredientes = mezclaDeIngredientes primeraPizza segundaPizza,
        calorias = calculoCalorias primeraPizza segundaPizza
    }

mezclaDeIngredientes primeraPizza segundaPizza = 
    agregarSinRepetir (ingredientes primeraPizza) (ingredientes segundaPizza)

calculoCalorias primeraPizza segundaPizza =
    calorias primeraPizza / 2 + calorias segundaPizza 

agregarSinRepetir base agregados = 
    foldl agregar base agregados
    where 
        agregar base agregado
            |notElem agregado base = agregado : base
            |otherwise = base


--eje,plos
billy= Pizza{ingredientes= ["papa"], tamanio = 4, calorias= 50}

{-}
--aux ingredienteRepetido
ingredienteRepetido :: String->Pizza -> Pizza
ingredienteRepetido ingrediente pizza
    | elem ingrediente $ ingredientes pizza = pizza
    | otherwise = ingredientes = pizza { ingredientes = (ingredienteNuevo:).ingredientes $ pizza}
-}

--Punto 5
-- Pedido es [ ]
satisfacccionXPedido :: [Pizza] -> Number
satisfacccionXPedido  = sum.map nivelDeSatisfaccion 

-- Punto 6
pizzeriaLosHijosDePato :: [Pizza] -> [Pizza]
pizzeriaLosHijosDePato pizza = pizza

-- punto 7 
sonDignasDeCalleCorrientes :: [Pizza]  ->[[Pizza] -> [Pizza]]->[[Pizza] -> [Pizza]]
sonDignasDeCalleCorrientes pedido pizzerias= filter (comparacionSatisfaccion pedido) pizzerias 
comparacionSatisfaccion pedido pizzeria= satisfacccionXPedido pedido < (satisfacccionXPedido.pizzeria) pedido



sonDignasDeCalleCorrientes' pedido pizzerias =
    filter (mejoraSatisfaccion' pedido) pizzerias
    where mejoraSatisfaccion' pedido pizzeria =
            nivelDeSatisfaccionPedido' pedido < (nivelDeSatisfaccionPedido' . pizzeria) pedido

nivelDeSatisfaccionPedido' :: [Pizza] -> Number
nivelDeSatisfaccionPedido'  = sum.map nivelDeSatisfaccion             

laPizzeriaPredilecta = foldl1 (.)