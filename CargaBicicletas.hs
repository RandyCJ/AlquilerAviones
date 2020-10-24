module CargaBicicletas where

-- Estructura bicicletas
type CodigoBic = String
type TipoBic = String
type UbicacionBic = String
data Bicicleta = Bicicleta CodigoBic TipoBic UbicacionBic;

-- Constructor bicicletas
crearBicicleta(elemento) = Bicicleta (elemento!!0) (elemento!!1) (elemento!!2)
getCodigo (Bicicleta codigo _ _) = codigo;
getTipo (Bicicleta _ tipo _) = tipo;
getUbicacion (Bicicleta _ _ ubicacion) = ubicacion;

--Muestra bicicletas
showBicicleta :: Bicicleta -> [Char]
showBicicleta bicicleta =
    let
        codigo = getCodigo(bicicleta)
        tipo = getTipo(bicicleta)
        ubicacion = getUbicacion(bicicleta)
    in
        "Codigo: " ++ codigo ++ ", Tipo: " ++ tipo ++ ", Ubicacion: " ++ ubicacion


showBicicletas :: [Bicicleta] -> IO ()
showBicicletas [] = print("")
showBicicletas listaBicicletas =
    do
        print(showBicicleta (head listaBicicletas))
        showBicicletas (tail listaBicicletas)

separaPorComas :: ([Char], [Char]) -> [[Char]]
separaPorComas (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
            [temp] ++ separaPorComas ((tail cadena), "")
        else
            separaPorComas ((tail cadena), temp ++ [(head cadena)])

separaElementos :: [[Char]] -> [Bicicleta]
separaElementos lista =
    if null(lista) then []
    else
        [crearBicicleta(separaPorComas((head lista), ""))] ++ separaElementos (tail lista)

leerArchivoBicicletas :: FilePath -> IO [Bicicleta]
leerArchivoBicicletas archivo = do
    contenido <- readFile archivo
    let bicicletas = separaElementos (lines contenido)
    return bicicletas
