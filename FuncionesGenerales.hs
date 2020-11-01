module FuncionesGenerales where
import Data.List


separaPorComas :: ([Char], [Char]) -> [[Char]]
separaPorComas (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
            [temp] ++ separaPorComas ((tail cadena), "")
        else
            separaPorComas ((tail cadena), temp ++ [(head cadena)])

obtenerDistancia :: Float -> Float -> Float -> Float -> Float
obtenerDistancia x1 y1 x2 y2 =
    sqrt ((x1-x2)**2 + (y1-y2)**2)
