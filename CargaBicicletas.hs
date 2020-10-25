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
showBicicleta :: Bicicleta -> IO ()
showBicicleta bicicleta =
    let
        codigo = getCodigo(bicicleta)
        tipo = getTipo(bicicleta)
        ubicacion = getUbicacion(bicicleta)
    in
        putStr("Codigo: " ++ codigo ++ ", Tipo: " ++ tipo ++ ", Ubicacion: " ++ ubicacion ++ "\n")

showBicicleta2 :: Bicicleta -> String -> IO ()
showBicicleta2 bicicleta parqueo =
    let
        codigo = getCodigo(bicicleta)
        tipo = getTipo(bicicleta)
        ubicacion = getUbicacion(bicicleta)
    in
        if ubicacion == parqueo then
            putStr("Codigo: " ++ codigo ++ ", Tipo: " ++ tipo ++ ", Ubicacion: " ++ ubicacion ++ "\n")
        else
            return ()

showBicicletas :: [Bicicleta] -> IO ()
showBicicletas [] = return()
showBicicletas listaBicicletas =
    do
        showBicicleta (head listaBicicletas)
        showBicicletas (tail listaBicicletas)

showBicisXParqueo :: [Bicicleta] -> String -> IO ()
showBicisXParqueo [] s = return()
showBicisXParqueo listaBicicletas parqueo =
    do
        showBicicleta2 (head listaBicicletas) parqueo
        showBicisXParqueo (tail listaBicicletas) parqueo


separaPorComas2 :: ([Char], [Char]) -> [[Char]]
separaPorComas2 (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
            [temp] ++ separaPorComas2 ((tail cadena), "")
        else
            separaPorComas2 ((tail cadena), temp ++ [(head cadena)])

separaBicicletas :: [[Char]] -> [Bicicleta]
separaBicicletas lista =
    if null(lista) then []
    else
        [crearBicicleta(separaPorComas2((head lista), ""))] ++ separaBicicletas (tail lista)

leerArchivoBicicletas :: FilePath -> IO [Bicicleta]
leerArchivoBicicletas archivo = do
    contenido <- readFile archivo
    let bicicletas = separaBicicletas (lines contenido)
    return bicicletas
