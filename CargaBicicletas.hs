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
--E: una bicicleta
--S: N/A
showBicicleta :: Bicicleta -> IO ()
showBicicleta bicicleta =
    let
        codigo = getCodigo(bicicleta)
        tipo = getTipo(bicicleta)
        ubicacion = getUbicacion(bicicleta)
    in
        putStr("Codigo: " ++ codigo ++ ", Tipo: " ++ tipo ++ ", Ubicacion: " ++ ubicacion ++ "\n")

--Muestra una bici si esta corresponde al nombre de parqueo que recibio
--E: una bicicleta, el nombre de un parqueo
--S: N/A
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

--Muestra todas las bicicletas del sistema
--E: lista de bicicletas
--S: N/A
showBicicletas :: [Bicicleta] -> IO ()
showBicicletas [] = return()
showBicicletas listaBicicletas =
    do
        showBicicleta (head listaBicicletas)
        showBicicletas (tail listaBicicletas)

--Muestra las bicicletas de un parqueo
--E: lista de bicicletas, nombre del parqueo
--S: N/A
showBicisXParqueo :: [Bicicleta] -> String -> IO ()
showBicisXParqueo [] s = return()
showBicisXParqueo listaBicicletas parqueo =
    do
        showBiciParqueo (head listaBicicletas) parqueo
        showBicisXParqueo (tail listaBicicletas) parqueo

--Separa las bicicletas de un archivo y las convierte en una lista
--E: una lista con listas de string
--S:una lista de bicicletas
separaBicicletas :: [[Char]] -> [Bicicleta]
separaBicicletas lista =
    if null(lista) then []
    else
        [crearBicicleta(separaPorComas((head lista), ""))] ++ separaBicicletas (tail lista)

--lee el archivo de bicicletas
--E: la ruta del archivo
--S: retorna una lista con bicicletas
leerArchivoBicicletas :: FilePath -> IO [Bicicleta]
leerArchivoBicicletas archivo = do
    file <- openFile archivo ReadWriteMode
    contenido <- hGetContents file
    let bicicletas = separaBicicletas (lines contenido)
    putStr (contenido)
    return bicicletas

--Muestra una bici si esta no esta en transito
--E: una bici
--S: N/A
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

--Muestra todas las bicis que no esten en transito
--E: lista de bicicletas
--S: N/A
showBicisDisponibles :: [Bicicleta] -> IO ()
showBicisDisponibles [] = return()
showBicisDisponibles listaBicicletas =
    do
        showBiciDisponible (head listaBicicletas)
        showBicisDisponibles (tail listaBicicletas)


--compara datos y retorna un objeto bicicleta
--E: codigo de la bici, lista de bicicletas
--S: una bicicleta
getBicicleta :: String -> [Bicicleta] -> Bicicleta
getBicicleta codBici lB = do
    let codBicicleta = getCodigo (head lB)

    if codBicicleta == codBici then
        head lB
    else
        getBicicleta codBici (tail lB)

--Cambia la ubicacion de una bicicleta, ya sea a transito o al nombre de un parqueo
--E: lista de bicicletas, la bicicleta a cambiar, la ubicacion a colocar
--S: retorna la lista de bicicletas ya actualizada
cambiarUbicacion :: [Bicicleta] -> Bicicleta -> String -> [Bicicleta]
cambiarUbicacion lB bicicleta ubicacion = do
    let codigoBici = getCodigo (head lB)
    let codBici = getCodigo (bicicleta)

    if codBici == codigoBici then
        do
            let tipoBici = getTipo (bicicleta)
            let nuevaBici = crearBicicleta([codBici, tipoBici, ubicacion])
            [nuevaBici] ++ (tail lB)
    else
        [head lB] ++ cambiarUbicacion (tail lB) bicicleta ubicacion

--Convierte una lista de bicicletas a un string que represente la lista
--E: lista de bicicletas, un string vacio donde se guardara el string
--S: un string
bicisAString :: [Bicicleta] -> String -> String
bicisAString [] s = s
bicisAString lB string = do
    let codigo = getCodigo (head lB)
    let tipo = getTipo (head lB)
    let ubicacion = getUbicacion (head lB)
    let nuevaBici = codigo ++ "," ++ tipo ++ "," ++ ubicacion ++ "\n"

    bicisAString (tail lB) (string ++ nuevaBici)

--sobreescribe el archivo con los nuevos datos actualizados
--E: recibe la ruta y el string a escribir
--S: N/A
escribirNuevosDatos :: String -> String -> IO ()
escribirNuevosDatos ruta datos = do
    writeFile ruta datos
    return ()