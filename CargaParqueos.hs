module CargaParqueos where
import CargaBicicletas
import FuncionesGenerales


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
            putStr("\nNombre: " ++ nombre ++ ", Direccion: " ++ direccion ++ ", Provincia: " ++ provincia ++ ", X: " ++ show ubx ++ ", Y: " ++ show uby ++ "\n")
            showBicisXParqueo lB nombre
        else
            if p == provincia then
                do
                putStr("\nNombre: " ++ nombre ++ ", Direccion: " ++ direccion ++ ", Provincia: " ++ provincia ++ ", X: " ++ show ubx ++ ", Y: " ++ show uby ++ "\n")
                showBicisXParqueo lB nombre
            else
                return ()

showParqueos :: [Parqueo] -> [Bicicleta] -> String -> IO ()
showParqueos [] b s = return()
showParqueos lP lB p =
    do
        showParqueo (head lP) lB p
        showParqueos (tail lP) lB p

show1Parqueo :: [Parqueo] -> [Bicicleta] -> String -> IO ()
show1Parqueo [] l s = return()
show1Parqueo lP lB nombreParqueo = 
    do
        let parqueo = getNombreParqueo (head lP)
        if parqueo == nombreParqueo then
            showParqueo (head lP) lB "TP"
        else
            show1Parqueo (tail lP) lB nombreParqueo

parqueoMasCercanoAux :: [Parqueo] -> [Bicicleta] -> IO ()
parqueoMasCercanoAux lP lB = do
    putStr "Ingrese su posicion x: "
    pX <- getLine
    let pXF = (read pX :: Float)
    putStr "Ingrese su posicion y: "
    pY <- getLine
    let pYF = (read pY :: Float)
    parqueoMasCercano lP lB pXF pYF 0 (head lP)

parqueoMasCercano :: [Parqueo] -> [Bicicleta] -> Float -> Float -> Float -> Parqueo -> IO ()
parqueoMasCercano [] lB _ _ distancia parqueo = showParqueo parqueo lB "TP"
parqueoMasCercano lP lB x y distancia parqueo = do
    let xP = getUbicacionX (head lP)
    let yP = getUbicacionY (head lP)
    let distanciaP = obtenerDistancia x y xP yP
    if distancia == 0 then
        parqueoMasCercano (tail lP) lB x y distanciaP (head lP)
    else
        if distanciaP < distancia then
            parqueoMasCercano (tail lP) lB x y distanciaP (head lP)
        else
            parqueoMasCercano (tail lP) lB x y distancia parqueo

        

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

