module Lib
    ( someFunc
    ) where

import Text.Show.Functions

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Tablero = [Celda]

data Celda = Celda{
    valor          ::   Int,
    filaCelda    :: Fila,
    columnaCelda :: Columna
} deriving(Show,Eq)

type Columna = Int
type Fila    = Int

data Cuadrado = Cuadrado{
    filaCuadrado :: Int,
    columnaCuadrado :: Int
}deriving(Show, Eq)

cuadradoDeCelda :: Celda -> Int
cuadradoDeCelda celda = definirCuadrado (Cuadrado (divisionConRedondeo (filaCelda celda) 3) (divisionConRedondeo (columnaCelda celda) 3))

divisionConRedondeo :: Int -> Int -> Int
divisionConRedondeo num1 num2
    | mod num1 num2 > 0 = div num1 num2 + 1
    | otherwise         = div num1 num2

definirCuadrado :: Cuadrado -> Int
definirCuadrado cuadrado
    | filaCuadrado cuadrado == 1 = columnaCuadrado cuadrado
    | filaCuadrado cuadrado == 2 = columnaCuadrado cuadrado + 3
    | filaCuadrado cuadrado == 3 = columnaCuadrado cuadrado + 6

type Condicion = Int -> Tablero -> Bool

seRepiteEnFila :: Fila -> Condicion
seRepiteEnFila fila valorCualquiera = any (flip tieneValorN valorCualquiera) . filter (flip esFilaN fila)

esFilaN :: Celda -> Int -> Bool
esFilaN celda numeroDeFila = numeroDeFila == filaCelda celda

tieneValorN :: Celda -> Int -> Bool
tieneValorN celda valorASaber = valorASaber == valor celda

seRepiteEnColumna :: Columna -> Condicion 
seRepiteEnColumna columna valorCualquiera = any (flip tieneValorN valorCualquiera) . filter (flip esColumnaN columna)

esColumnaN :: Celda -> Int -> Bool
esColumnaN celda numeroDeColumna = numeroDeColumna == columnaCelda celda

perteneceAlCuadrado :: Celda -> Int -> Bool
perteneceAlCuadrado celda numeroDeCuadrado = cuadradoDeCelda celda == numeroDeCuadrado

seRepiteEnCuadrado :: Int -> Condicion
seRepiteEnCuadrado cuadrado valorCualquiera = any (flip tieneValorN valorCualquiera) . filter (flip perteneceAlCuadrado cuadrado)

probarValoresEnCelda :: Celda -> Tablero -> [Int]
probarValoresEnCelda celda tablero = probarValoresEnCelda' celda tablero 1 []

probarValoresEnCelda' :: Celda -> Tablero -> Int -> [Int] -> [Int]
probarValoresEnCelda' celda tablero valorAProbar listaNumerosPosibles
    | valorAProbar > 9                               = listaNumerosPosibles
    | puedeIrValorEnCelda valorAProbar celda tablero = probarValoresEnCelda' celda tablero (valorAProbar + 1) (valorAProbar:listaNumerosPosibles)
    | otherwise                                      = probarValoresEnCelda' celda tablero (valorAProbar + 1) listaNumerosPosibles

puedeIrValorEnCelda :: Int -> Celda -> Tablero -> Bool
puedeIrValorEnCelda numero celda tablero = not (seRepiteEnFila (filaCelda celda) numero tablero || seRepiteEnColumna (columnaCelda celda) numero tablero || seRepiteEnCuadrado (cuadradoDeCelda celda) numero tablero)

agregarValorACelda :: Int -> Celda -> Celda
agregarValorACelda numero celda = celda {valor = numero}

agregarValorACeldaSiCumple :: Celda -> Tablero -> Celda
agregarValorACeldaSiCumple celda tablero
    | length (probarValoresEnCelda celda tablero) == 1 = agregarValorACelda (head (probarValoresEnCelda celda tablero)) celda
    | otherwise                                        = celda

cambiarCeldaEnTablero :: Celda -> Tablero -> Tablero
cambiarCeldaEnTablero celda = map (cambiarValorSiEsLaCelda celda) 

cambiarValorSiEsLaCelda :: Celda -> Celda -> Celda
cambiarValorSiEsLaCelda celdaConValor celdaDelTablero
    | esLaCelda celdaConValor celdaDelTablero = flip agregarValorACelda celdaDelTablero (valor celdaConValor)
    | otherwise                               = celdaDelTablero

esLaCelda :: Celda -> Celda -> Bool
esLaCelda celdaQueBusco celdaDelTablero = filaCelda celdaQueBusco == filaCelda celdaDelTablero && columnaCelda celdaQueBusco == columnaCelda celdaDelTablero  

resolverCelda :: Celda -> Tablero -> Tablero
resolverCelda celda tablero = flip cambiarCeldaEnTablero tablero . flip agregarValorACeldaSiCumple tablero $ celda

convertirListaDeValoresEnTablero :: [Int] -> Tablero
convertirListaDeValoresEnTablero valores = zipWith generarCeldaAPartirDePosicionesYValores (reverse listaDePosicionesEnOrden) valores 

type Posicion = (Fila, Columna)

generarCeldaAPartirDePosicionesYValores :: Posicion -> Int -> Celda
generarCeldaAPartirDePosicionesYValores (fila, columna) numero = Celda numero fila columna 

listaDePosicionesEnOrden :: [Posicion]
listaDePosicionesEnOrden = generarPosiciones 1 []

generarPosiciones :: Int -> [Posicion] -> [Posicion]
generarPosiciones iteracion posiciones 
    | iteracion > 81 = posiciones 
    | otherwise      = generarPosiciones (iteracion + 1) [generarPosicion iteracion] ++ posiciones 

generarPosicion :: Int -> Posicion
generarPosicion valorPosicion 
    | valorPosicion > 9 * 8 = (9, valorPosicion - 9 * 8)
    | valorPosicion > 9 * 7 = (8, valorPosicion - 9 * 7)
    | valorPosicion > 9 * 6 = (7, valorPosicion - 9 * 6)
    | valorPosicion > 9 * 5 = (6, valorPosicion - 9 * 5)
    | valorPosicion > 9 * 4 = (5, valorPosicion - 9 * 4)
    | valorPosicion > 9 * 3 = (4, valorPosicion - 9 * 3)
    | valorPosicion > 9 * 2 = (3, valorPosicion - 9 * 2)
    | valorPosicion > 9 * 1 = (2, valorPosicion - 9 * 1)
    | otherwise             = (1, valorPosicion)

tableroTieneCeros :: Tablero -> Bool
tableroTieneCeros = any celdaSinResolver

celdaSinResolver :: Celda -> Bool
celdaSinResolver celda = valor celda == 0

resolverTablero :: Tablero -> Tablero
resolverTablero tablero = resolverTablero' tablero 1 

resolverTablero' :: Tablero -> Int -> Tablero
resolverTablero' tablero iteracion 
    | not (tableroTieneCeros tablero) = tablero
    | iteracion > 81                  = resolverTablero' tablero 1
    | otherwise                       = resolverTablero' (resolverCeldaSiTieneCero (celdaConPosicion iteracion tablero) tablero) (iteracion + 1)

resolverCeldaSiTieneCero :: Celda -> Tablero -> Tablero
resolverCeldaSiTieneCero celda tablero
    | celdaSinResolver celda = resolverCelda celda tablero
    | otherwise              = tablero

celdaConPosicion :: Int -> Tablero -> Celda
celdaConPosicion posicion tablero = head . filter (esLaCelda (generarCeldaAPartirDePosicionesYValores (generarPosicion posicion) 0)) $ tablero

listaDeValores :: [Int]
listaDeValores = [0,8,0,5,7,6,2,0,0,0,0,0,4,0,2,0,0,0,0,0,0,0,3,9,5,4,8,6,3,0,9,0,0,8,5,2,0,9,0,2,0,0,3,7,0,8,0,0,0,5,0,6,9,4,2,5,7,6,0,3,4,8,9,3,0,8,7,0,0,0,2,5,0,4,0,0,0,0,0,0,6]

tableroPrueba = convertirListaDeValoresEnTablero listaDeValores