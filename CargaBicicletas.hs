module CargaBicicletas where
import FuncionesGenerales
import System.IO


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

showBiciParqueo :: Bicicleta -> String -> IO ()
showBiciParqueo bicicleta parqueo =
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
        showBiciParqueo (head listaBicicletas) parqueo
        showBicisXParqueo (tail listaBicicletas) parqueo


separaBicicletas :: [[Char]] -> [Bicicleta]
separaBicicletas lista =
    if null(lista) then []
    else
        [crearBicicleta(separaPorComas((head lista), ""))] ++ separaBicicletas (tail lista)

leerArchivoBicicletas :: FilePath -> IO [Bicicleta]
leerArchivoBicicletas archivo = do
    file <- openFile archivo ReadWriteMode
    contenido <- hGetContents file
    let bicicletas = separaBicicletas (lines contenido)
    putStr (contenido)
    return bicicletas

showBiciDisponible :: Bicicleta -> IO ()
showBiciDisponible bicicleta =
    let
        codigo = getCodigo(bicicleta)
        tipo = getTipo(bicicleta)
        ubicacion = getUbicacion(bicicleta)
    in
        if ubicacion /= "transito" then
            putStr("Codigo: " ++ codigo ++ ", Tipo: " ++ tipo ++ ", Ubicacion: " ++ ubicacion ++ "\n")
        else
            return ()

showBicisDisponibles :: [Bicicleta] -> IO ()
showBicisDisponibles [] = return()
showBicisDisponibles listaBicicletas =
    do
        showBiciDisponible (head listaBicicletas)
        showBicisDisponibles (tail listaBicicletas)


getBicicleta :: String -> [Bicicleta] -> Bicicleta
getBicicleta codBici lB = do
    let codBicicleta = getCodigo (head lB)

    if codBicicleta == codBici then
        head lB
    else
        getBicicleta codBici (tail lB)


cambiarUbicacion :: [Bicicleta] -> Bicicleta -> [Bicicleta]
cambiarUbicacion lB bicicleta = do
    let codigoBici = getCodigo (head lB)
    let codBici = getCodigo (bicicleta)

    if codBici == codigoBici then
        do
            let tipoBici = getTipo (bicicleta)
            let nuevaBici = crearBicicleta([codBici, tipoBici, "en transito"])
            [nuevaBici] ++ (tail lB)
    else
        [head lB] ++ cambiarUbicacion (tail lB) bicicleta
    
bicisAString :: [Bicicleta] -> String -> String
bicisAString [] s = s
bicisAString lB string = do
    let codigo = getCodigo (head lB)
    let tipo = getTipo (head lB)
    let ubicacion = getUbicacion (head lB)
    let nuevaBici = codigo ++ "," ++ tipo ++ "," ++ ubicacion ++ "\n"

    bicisAString (tail lB) (string ++ nuevaBici)

escribirNuevosDatos :: String -> String -> IO ()
escribirNuevosDatos ruta datos = do
    writeFile ruta datos
    return ()