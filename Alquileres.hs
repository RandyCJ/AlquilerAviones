module Alquileres where
import CargaUsuarios
import FuncionesGenerales
import System.IO


solicitarCedula :: [Usuario] -> IO (Integer)
solicitarCedula lU = do
    putStr "\nIngrese su cedula: "
    ced <- getLine
    let cedInt = (read ced :: Integer)

    if (verificarCedula lU cedInt) == 0 then
        return cedInt
    else
        do
        putStr "\nLa cedula que ingreso no existe, favor ingrese nuevamente"
        solicitarCedula lU

verificarCedula :: [Usuario] -> Integer -> Integer
verificarCedula [] i = 1
verificarCedula lU cedula = do
    let cedulaA = getCedula (head lU)
    if cedulaA == cedula then
        0
    else
        verificarCedula (tail lU) cedula



-- Estructura Alquiler
type Identificador = Integer
type Estado = String
type CedulaUsuario = Integer
type ParqueoSalida = String
type ParqueoLlegada = String
type CodigoBicicleta = String
type TipoBici = String
data Alquiler = Alquiler Identificador Estado CedulaUsuario ParqueoSalida ParqueoLlegada CodigoBicicleta TipoBici;

-- Constructor Alquileres
crearAlquiler(elemento) = Alquiler (read (elemento!!0) :: Integer) (elemento!!1) (read (elemento!!2) :: Integer) (elemento!!3) (elemento!!4) (elemento!!5) (elemento!!6)
getIdentificador (Alquiler identificador _ _ _ _ _ _ ) = identificador;
getEstado (Alquiler _ estado _ _ _ _ _ ) = estado;
getCedulaUsuario(Alquiler _ _ cedula _ _ _ _ ) = cedula;
getParqueoSalida (Alquiler _ _ _ parqueoS _ _ _ ) = parqueoS;
getParqueoLlegada (Alquiler _ _ _ _ parqueo _ _ ) = parqueo;
getCodigoBici (Alquiler _ _ _ _ _ codigoBici _  ) = codigoBici;
getTipoBici (Alquiler _ _ _ _ _ _ tipoBici ) = tipoBici;

showAlquiler :: Alquiler -> IO ()
showAlquiler alquiler =
    let
        id = getIdentificador(alquiler)
        estado = getEstado(alquiler)
        cedUsuario = getCedulaUsuario(alquiler)
        parqueoSalida = getParqueoSalida(alquiler)
        parqueoLlegada = getParqueoLlegada(alquiler)
        codigoBici = getCodigoBici(alquiler)
        tipoBici = getTipoBici(alquiler)
    in
        putStr("ID: " ++ show id ++ ", estado: " ++ estado ++ ", cedula: " ++ show cedUsuario ++ ", parqueo llegada: " ++ parqueoLlegada ++ ", parqueo salida: " ++ parqueoSalida ++ ", Codigo bici: " ++ codigoBici ++ ", Tipo: " ++ tipoBici ++ "\n")

showAlquileres :: [Alquiler] -> IO ()
showAlquileres [] = return ()
showAlquileres lA = do
    showAlquiler (head lA)
    showAlquileres (tail lA)

separaAlquileres :: [[Char]] -> [Alquiler]
separaAlquileres lista =
    if null(lista) then []
    else
        [crearAlquiler(separaPorComas((head lista), ""))] ++ separaAlquileres (tail lista)


leerArchivoAlquileres :: FilePath -> IO [Alquiler]
leerArchivoAlquileres archivo = do
    hdl <- openFile archivo ReadMode
    contenido <- hGetContents hdl
    let alquileres = separaAlquileres (lines contenido)
    escribirArchivo "alqui.txt" contenido
    return alquileres

escribirArchivo :: String -> String -> IO ()
escribirArchivo ruta lista = do
    writeFile ruta lista
    return ()


showAlquileresXUsuario :: [Alquiler] -> Integer -> IO ()
showAlquileresXUsuario [] c = return ()
showAlquileresXUsuario lA cedula = do
    let cedulaAlquiler = getCedulaUsuario (head lA)

    if cedula == cedulaAlquiler then
        do
        showAlquiler (head lA)
        showAlquileresXUsuario (tail lA) cedula
    else
        showAlquileresXUsuario (tail lA) cedula