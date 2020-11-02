module FuncionesGenerales where
import Data.List

--Separa un string con comas en una lista de string
--E: la lista de strings a separar, junto con un string vacio donde se va guardando cada linea
--S: retorna una lista con listas de string ya separados
separaPorComas :: ([Char], [Char]) -> [[Char]]
separaPorComas (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
            [temp] ++ separaPorComas ((tail cadena), "")
        else
            separaPorComas ((tail cadena), temp ++ [(head cadena)])

--Obtiene la distancia entre dos puntos
--E: recibe los puntos x y y de ambos lugares
--S: retorna la distancia entre los dos lugares
obtenerDistancia :: Float -> Float -> Float -> Float -> Float
obtenerDistancia x1 y1 x2 y2 =
    sqrt ((x1-x2)**2 + (y1-y2)**2)
