module CargaParqueos where

-- Estructura parqueos
type NombreParqueo = String
type DireccionParqueo = String
type Provincia = String
type UbicacionX = Float
type UbicacionY = Float
data Parqueo = Parqueo NombreParqueo DireccionParqueo Provincia UbicacionX UbicacionY;

-- Constructor parqueos
crearParqueo(elemento) = Parqueo (elemento!!0) (elemento!!1) (elemento!!2) (read (elemento!!3) :: Float) (read (elemento!!4) :: Float)
getNombreParqueo (Parqueo nombre _ _ _ _) = nombre;
getDireccionParqueo (Parqueo _ direccion _ _ _) = direccion;
getProvinciaParqueo (Parqueo _ _ provincia _ _) = provincia;
getUbicacionX (Parqueo _ _ _ ubicacionx _) = ubicacionx;
getUbicacionY (Parqueo _ _ _ _ ubicaciony) = ubicaciony;

--Muestra Parqueos
showParqueo :: Parqueo -> [Char]
showParqueo parqueo =
    let
        nombre = getNombreParqueo(parqueo)
        direccion = getDireccionParqueo(parqueo)
        provincia = getProvinciaParqueo(parqueo)
        ubx = getUbicacionX(parqueo)
        uby = getUbicacionY(parqueo)
    in
        "Nombre: " ++ nombre ++ ", Direccion: " ++ direccion ++ ", Provincia: " ++ provincia ++ ", X: " ++ show ubx ++ ", Y: " ++ show uby


showParqueos :: [Parqueo] -> IO ()
showParqueos [] = print("")
showParqueos listaParqueos =
    do
        print(showParqueo (head listaParqueos))
        showParqueos (tail listaParqueos)

separaPorComas :: ([Char], [Char]) -> [[Char]]
separaPorComas (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
            [temp] ++ separaPorComas ((tail cadena), "")
        else
            separaPorComas ((tail cadena), temp ++ [(head cadena)])

separaElementos :: [[Char]] -> [Parqueo]
separaElementos lista =
    if null(lista) then []
    else
        [crearParqueo(separaPorComas((head lista), ""))] ++ separaElementos (tail lista)

leerArchivoParqueos :: FilePath -> IO [Parqueo]
leerArchivoParqueos archivo = do
    contenido <- readFile archivo
    let parqueos = separaElementos (lines contenido)
    return parqueos

