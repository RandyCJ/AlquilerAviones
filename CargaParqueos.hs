module CargaParqueos where
import CargaBicicletas

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
showParqueo :: Parqueo -> [Bicicleta] -> String -> IO ()
showParqueo parqueo lB p =
    let
        nombre = getNombreParqueo(parqueo)
        direccion = getDireccionParqueo(parqueo)
        provincia = getProvinciaParqueo(parqueo)
        ubx = getUbicacionX(parqueo)
        uby = getUbicacionY(parqueo)
    in
        if p == "TP" then
            do
            print("Nombre: " ++ nombre ++ ", Direccion: " ++ direccion ++ ", Provincia: " ++ provincia ++ ", X: " ++ show ubx ++ ", Y: " ++ show uby)
            showBicicletas2 lB nombre
        else
            if p == provincia then
                do
                print("Nombre: " ++ nombre ++ ", Direccion: " ++ direccion ++ ", Provincia: " ++ provincia ++ ", X: " ++ show ubx ++ ", Y: " ++ show uby)
                showBicicletas2 lB nombre
            else
                return ()

showParqueos :: [Parqueo] -> [Bicicleta] -> String -> IO ()
showParqueos [] b s = return()
showParqueos lP lB p =
    do
        showParqueo (head lP) lB p
        showParqueos (tail lP) lB p
 

separaPorComas :: ([Char], [Char]) -> [[Char]]
separaPorComas (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
            [temp] ++ separaPorComas ((tail cadena), "")
        else
            separaPorComas ((tail cadena), temp ++ [(head cadena)])

separaParqueos :: [[Char]] -> [Parqueo]
separaParqueos lista =
    if null(lista) then []
    else
        [crearParqueo(separaPorComas((head lista), ""))] ++ separaParqueos (tail lista)

leerArchivoParqueos :: FilePath -> IO [Parqueo]
leerArchivoParqueos archivo = do
    contenido <- readFile archivo
    let parqueos = separaParqueos (lines contenido)
    return parqueos

