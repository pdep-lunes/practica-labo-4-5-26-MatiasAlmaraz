module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)

type Juguete = String

data Perro = UnPerro
{
    raza :: String,
    juguetes :: [Juguete],
    tiempo :: Number,
    energia :: Number
} deriving (Show)

modificarEnergia :: (Number -> Number) -> Perro -> Perro
modificarEnergia unaFuncion unPerro = max 0 . unaFuncion . energia $ unPerro

jugar :: Perro -> Perro
jugar unPerro = modificarEnergia (subtract  10) unPerro

ladrar :: Number -> Perro -> Perro
ladrar unosLadridos unPerro = modificarEnergia (+unosLadridos/2) unPerro

regalar :: Juguete -> Perro -> Perro
regalar unPerro unJuguete = unPerro{ juguetes = juguetes unPerro ++ [unJuguete] }

esRazaExtravagante :: Perro -> Bool
esRazaExtravagante unPerro = raza unPerro == "dalmata" || raza unPerro == "pomerania"

diaDeSpa :: Perro -> Perro
diaDeSpa unPerro 
    | tiempo unPerro >= 50 || esRazaExtravagante unPerro = unPerro{ energia = 100, regalar unPerro "perine de goma" }
    | otherwise = unPerro

cantidadDeJuguetes :: Perro -> Number
cantidadDeJuguetes unPerro = length . juguetes $ unPerro

diaDeCampo :: Perro -> Perro
diaDeCampo unPerro
    | (cantidadDeJuguetes unPerro) > 0 = jugar unPerro{ juguetes = tail . juguetes $ unPerro }
    | otherwise = unPerro

zara :: Perro
zara = UnPerro{ nombre = "Zara", juguetes = ["pelota", "mantita"], tiempo = 90, energia = 80}

type Tiempo = Number
type Actividad = (Perro -> Perro, Tiempo)
data Guarderia = UnaGuarderia
{
    actividades :: [Actividad]
} deriving (Show)

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = UnaGuarderia{ actividades = [ (jugar, 30), (ladrar 18, 20), (regalar "pelota", 0), (diaDeSpa, 120), (diaDeCampo, 720) ] }

cantidadDeTiempo :: Actividad -> Tiempo
cantidadDeTiempo unaActividad = snd unaActividad

permitirPerro :: Perro -> Guarderia -> Bool
permitirPerro unPerro unaGuarderia = tiempo unPerro >= sum . map snd . actividades $ unaGuarderia

esPerroResponsable :: Perro -> Bool
esPerroResponsable unPerro = (cantidadDeJuguetes . diaDeCampo $ unPerro) > 3
