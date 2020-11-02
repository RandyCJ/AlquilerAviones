module Alquileres where
import CargaUsuarios
import FuncionesGenerales
import System.IO

--Solicita una cedula al usuario
--E: la lista con usuarios
--S: retorna la cedula ingresada por el usuario
--Funcionamiento: pide la cedula hasta que se ingrese una que exista y la retorna
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

--Verifica que una cedula exista
--E: la lista de usuarios y una cedula para verificas
--S: retorna un 0 si la cedula existe, 1 si no existe
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

--Muestra un alquiler
--E: un alquiler
--S: N/A
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

--Muestra todos los alquileres
--E: lista de alquileres
--S: N/A
showAlquileres :: [Alquiler] -> IO ()
showAlquileres [] = return ()
showAlquileres lA = do
    showAlquiler (head lA)
    showAlquileres (tail lA)

--Separa las lineas de un archivo y crea una lista con alquileres
--E: una lista con listas de strings
--S: una lista de alquileres
separaAlquileres :: [[Char]] -> [Alquiler]
separaAlquileres lista =
    if null(lista) then []
    else
        [crearAlquiler(separaPorComas((head lista), ""))] ++ separaAlquileres (tail lista)

--Lee el archivo con alquileres, lo separa por lineas
--E: la ruta del archivo
--S: una lista de alquileres
leerArchivoAlquileres :: FilePath -> IO [Alquiler]
leerArchivoAlquileres archivo = do
    file <- openFile archivo ReadWriteMode
    contenido <- hGetContents file
    let alquileres = separaAlquileres (lines contenido)

    putStr (contenido ++ "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
    --escribirArchivo "al.txt" contenido
    return alquileres

--Escribe en un archivo, para cuando se necesite actualizar un valor
--E: Recibe un string, el que se va a escribir en el archivo, y la ruta del archivo
escribirArchivo :: String -> String -> IO ()
escribirArchivo ruta lista = do
    writeFile ruta lista
    return ()

--Escribe en el archivo de alquileres el nuevo alquiler
--E: recibe un string que representa el alquiler
--S: N/A
agregarAlquiler :: String -> IO ()
agregarAlquiler alquiler = do
    appendFile "al.txt" (alquiler ++ "\n")
    return ()
    
--Muestra los alquileres de un usuario
--E: lista de alquileres, cedula de un usuario
--S: N/A
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

--Cambia el estado de un alquiler de activo a facturado
--E: lista de alquileres, id de alquiler
--S: retorna la lista con alquileres con el valor ya cambiado
cambiarEstado :: [Alquiler] -> Integer -> [Alquiler]
cambiarEstado lA idAlquiler = do
    let codAlquiler = getIdentificador (head lA)

    if codAlquiler == idAlquiler then
        do
        let cedUsuario = getCedulaUsuario (head lA)
        let pS = getParqueoSalida (head lA)
        let pL = getParqueoLlegada (head lA)
        let codBici = getCodigoBici (head lA)
        let tipoBici = getTipoBici (head lA)
        let nuevoAlquiler = crearAlquiler([show idAlquiler, "facturado", show cedUsuario, pS, pL, codBici, tipoBici])
        [nuevoAlquiler] ++ (tail lA)
    else
        [head lA] ++ cambiarEstado (tail lA) idAlquiler

--Convierte la lista de alquileres a un solo string
--E: lista de alquileres, un string vacio donde se ira almacenando el string completo
--S: Retorna el string ya creado de la lista
alquilerAString :: [Alquiler] -> String -> String
alquilerAString [] s = s
alquilerAString lA string = do
    let id = getIdentificador (head lA)
    let estado = getEstado (head lA)
    let cedUsuario = getCedulaUsuario (head lA)
    let pS = getParqueoSalida (head lA)
    let pL = getParqueoLlegada (head lA)
    let codBici = getCodigoBici (head lA)
    let tipoBici = getTipoBici (head lA)
    let nuevoAlquiler = show id ++ "," ++ estado ++ "," ++ show cedUsuario ++ "," ++ pS ++ "," ++ pL ++ "," ++ codBici ++ "," ++ tipoBici ++ "\n"

    alquilerAString (tail lA) (string ++ nuevoAlquiler)

--Escribe en un archivo, para cuando se necesite actualizar un valor
--E: Recibe un string, el que se va a escribir en el archivo, y la ruta del archivo
reescribirAlquileres :: String -> IO ()
reescribirAlquileres datos = do
    writeFile "al.txt" datos
    return ()