module Facturas where
import Alquileres
import CargaParqueos
import FuncionesGenerales
import System.IO

-- Estructura Factura
type IDFactura = Integer
type PSalida = String
type PDestino = String
type CedUsuarioF = Integer
type CodBiciF = String
type TipoBiciF = String
type Kilometros = Float
type Tarifa = Float
type MontoTotal = Float
data Factura = Factura IDFactura PSalida PDestino CedUsuarioF CodBiciF TipoBiciF Kilometros Tarifa MontoTotal;

-- Constructor Factura
crearFactura(elemento) = Factura (read (elemento!!0) :: Integer) (elemento!!1) (elemento!!2) (read (elemento!!3) :: Integer) (elemento!!4) (elemento!!5) (read (elemento!!6) :: Float) (read (elemento!!7) :: Float) (read (elemento!!8) :: Float)
getIDFactura (Factura id _ _ _ _ _ _ _ _) = id;
getPSalida (Factura _ pS _ _ _ _ _ _ _) = pS;
getPDestino (Factura _ _ pD _ _ _ _ _ _) = pD;
getCedUsuarioF (Factura _ _ _ cF _ _ _ _ _) = cF;
getCodBiciF (Factura _ _ _ _ cBF _ _ _ _) = cBF;
getTipoBiciF (Factura _ _ _ _ _ tBF _ _ _) = tBF;
getKilometros (Factura _ _ _ _ _ _ k _ _) = k;
getTarifa (Factura _ _ _ _ _ _ _ t _) = t;
getMontoTotal (Factura _ _ _ _ _ _ _ _ mT) = mT;

showFactura:: [Factura] -> Integer -> IO ()
showFactura [] i = do
    putStr "El codigo de factura que ingreso no existe"
    return ()
showFactura lF codigoFactura = do
    let codF = getIDFactura (head lF)

    if codF == codigoFactura then
        showFacturaAux (head lF)
    else
        showFactura (tail lF) codigoFactura

showFacturaAux :: Factura -> IO ()
showFacturaAux factura = do
    let codigoFactura = getIDFactura factura
    let pSalida = getPSalida factura
    let pDestino = getPDestino factura
    let cedUsuario = getCedUsuarioF factura
    let codBici = getCodBiciF factura
    let tipoBici = getTipoBiciF factura
    let kilometros = getKilometros factura
    let tarifa = getTarifa factura
    let montoTotal = getMontoTotal factura

    
    putStr ("\n\nCodigo de Factura: " ++ show codigoFactura)
    putStr ("\nParqueo de salida: " ++ pSalida)
    putStr ("\nParqueo destino: " ++ pDestino)
    putStr ("\nCedula: " ++ show cedUsuario)
    putStr ("\nCodigo de Bici: " ++ codBici)
    putStr ("\nTipo de bici: " ++ tipoBici)
    putStr ("\nTotal de kilometros: " ++ show kilometros)
    putStr ("\nTarifa: " ++ show tarifa)
    putStr ("\nMonto total: " ++ show montoTotal)
    putStr "\n--------------------------------------\n"


nuevaFactura :: [Alquiler] -> Int -> Integer -> Float -> Float -> [Parqueo] -> IO [Factura]
nuevaFactura [] _ _ _ _ _ = do
    putStr "\nEl codigo de alquiler que ingreso no existe\n"
    return []
nuevaFactura lA idFactura idAlquiler precioAE precioTR lP = do
    let codAlquiler = getIdentificador (head lA)

    if idAlquiler == codAlquiler then do
        let estado = getEstado (head lA)
        if estado == "facturado" then do
            putStr "\nEl codigo de alquiler que ingreso ya se encuentra facturado\n"
            return []
        else
            do
            let tipoBici = getTipoBici (head lA)
            if tipoBici == "AE" then
                return [nuevaFacturaAux (head lA) precioAE idFactura lP]
            else
                return [nuevaFacturaAux (head lA) precioTR idFactura lP]
    else
        nuevaFactura (tail lA) idFactura idAlquiler precioAE precioTR lP

nuevaFacturaAux :: Alquiler -> Float -> Int -> [Parqueo] -> Factura
nuevaFacturaAux alquiler precio idFactura lP = do
    
    let idALQ = getIdentificador alquiler
    let nombrePL = getParqueoLlegada alquiler
    let nombrePS = getParqueoSalida alquiler
    let parqueoLlegada = getParqueo nombrePL lP
    let parqueoSalida = getParqueo nombrePS lP
    let cedUsuario = getCedulaUsuario alquiler
    let codBici = getCodigoBici alquiler
    let tipoBici = getTipoBici alquiler
    let xPL = getUbicacionX (parqueoLlegada)
    let yPL = getUbicacionY (parqueoLlegada)
    let xPS = getUbicacionX (parqueoSalida)
    let yPS = getUbicacionY (parqueoSalida)
    let kilometros = obtenerDistancia xPL yPL xPS yPS
    let montoTotal = (precio*kilometros)
    crearFactura [show idALQ, nombrePS, nombrePL, show cedUsuario, codBici, tipoBici, show kilometros, show precio, show montoTotal]

showFacturas :: [Factura] -> IO ()
showFacturas [] = return ()
showFacturas lF = do

    showFacturaAux (head lF)
    showFacturas (tail lF)


show1Factura :: [Factura] -> Integer -> String -> String -> String -> IO ()
show1Factura [] _ _ _ _= do
    putStr "\nEl codigo que ingreso no se encuentra en el sistema\n"
    return ()
show1Factura lF codigo nombreEmpresa sitioWeb contacto = do
    let codigoFactura = getIDFactura (head lF)

    if codigoFactura == codigo then do
        putStr "\n--------------------------------------"
        putStr ("\nEmpresa: " ++ nombreEmpresa)
        putStr ("\nSitio Web: " ++ sitioWeb)
        putStr ("\nContacto: " ++ contacto)
        showFacturaAux (head lF)
    else
        show1Factura (tail lF) codigo nombreEmpresa sitioWeb contacto

separaFacturas :: [[Char]] -> [Factura]
separaFacturas lista =
    if null(lista) then []
    else
        [crearFactura(separaPorComas((head lista), ""))] ++ separaFacturas (tail lista)


leerArchivoFacturas :: FilePath -> IO [Factura]
leerArchivoFacturas archivo = do
    file <- openFile archivo ReadWriteMode
    contenido <- hGetContents file
    let facturas = separaFacturas (lines contenido)

    putStr (contenido ++ "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
    --escribirArchivo "al.txt" contenido
    return facturas

agregarFactura :: Factura -> IO ()
agregarFactura factura = do
    let idFactura = getIDFactura factura
    let nombrePS = getPSalida factura
    let nombrePL = getPDestino factura
    let cedUsuario = getCedUsuarioF factura
    let codBici = getCodBiciF factura
    let tipoBici = getTipoBiciF factura
    let kilometros = getKilometros factura
    let precio = getTarifa factura
    let montoTotal = getMontoTotal factura
    let string = show idFactura ++ "," ++ nombrePS ++ "," ++ nombrePL ++ "," ++ show cedUsuario ++ "," ++ codBici ++ "," ++ tipoBici ++ "," ++ show kilometros ++ "," ++ show precio ++ "," ++ show montoTotal ++ "\n"
    appendFile "fa.txt" string
    return ()

