import System.IO
import CargaParqueos
import CargaUsuarios
import CargaBicicletas
import Alquileres
import FuncionesGenerales
import Estadisticas
import Facturas
import Data.List

-- Datos de la empresa
nombreEmpresa = "Bici Alquiler Porfirio Lopez"
sitioWeb = "bicisporfirio.com"
contacto = "27591648"
tarifaPedal = 400
tarifaElectrico = 700

--Funcion encargada de facturar un alquiles
--E: listas con parqueos, bicicletas, usuarios, alquileres, las bicicletas del archivo, las facturas y la ruta del archivo de las bicicletas
--S: retorna una tupla con los valores actualizados de las listas recibidas en las entradas
-- Funcionamiento: guarda y actualiza los valores de las listas que recibe, y escribe en los archivos para la persistencia de datos
facturarAlquiler p b u a bA f rutaB = do
    putStr "Ingrese el codigo del alquiler a facturar: "
    id <- getLine
    let idAlquiler = (read id :: Integer)
    let idFactura = (length f) + 1
    facturaNueva <- nuevaFactura a idFactura idAlquiler tarifaElectrico tarifaPedal p

    
    if null(facturaNueva) then
        return (p, b, u, a, bA, f)
    else
        do
        let listaNuevaFacturas = f ++ facturaNueva
        let biciObjeto = getBicicleta (getCodBiciF (head facturaNueva)) b        
        let parqueoDestino = getPDestino (head facturaNueva)
        let listaNuevaBicicleta = cambiarUbicacion b biciObjeto parqueoDestino
        let listaBicisArchivo = cambiarUbicacion bA biciObjeto parqueoDestino
        let archivoBicis = bicisAString listaBicisArchivo ""
        let listaNuevaAlquileres = cambiarEstado a idAlquiler
        let archivoAlquileres = alquilerAString listaNuevaAlquileres ""
        
        escribirNuevosDatos rutaB archivoBicis
        reescribirAlquileres archivoAlquileres
        agregarFactura (head facturaNueva)

        putStr "\n--------------------------------------"
        putStr ("\nEmpresa: " ++ nombreEmpresa)
        putStr ("\nSitio Web: " ++ sitioWeb)
        putStr ("\nContacto: " ++ contacto)
        showFacturaAux (head facturaNueva)

        return (p, listaNuevaBicicleta, u, listaNuevaAlquileres, listaBicisArchivo, listaNuevaFacturas)

--Funcion encargada de alquilar una bici
--E: listas con parqueos, bicicletas, usuarios, alquileres, las bicicletas del archivo, la ruta del archivo de las bicicletas y las facturas
--S: retorna una tupla con los valores actualizados de las listas recibidas en las entradas
-- Funcionamiento: guarda y actualiza los valores de las listas que recibe, y escribe en los archivos para la persistencia de datos
alquilarBici p b u a rutaB bA f = do
    cedula <- solicitarCedula u
    parqueoObjeto <- parqueoMasCercanoAux p b
    putStr "Este es el parqueo mas cercano de salida\n"
    putStr "\nIndique el codigo de la bici de su preferencia:"
    bicicleta <- getLine
    let biciObjeto = getBicicleta bicicleta b
    putStr "\nParqueos de llegada:\n"
    showParqueosSOLOS p
    parqueoL <- solicitarParqueo p (getNombreParqueo parqueoObjeto)
    let codigoAlquiler = length a + 1
    let parametros = [show codigoAlquiler, "activo", show cedula, getNombreParqueo parqueoObjeto, parqueoL, bicicleta, getTipo biciObjeto]
    let stringParametros = (show codigoAlquiler ++ "," ++ "activo" ++ "," ++ show cedula ++ "," ++ getNombreParqueo parqueoObjeto ++ "," ++ parqueoL ++ "," ++ bicicleta ++ "," ++ parametros!!6)
    agregarAlquiler stringParametros
    let listaNuevaAlquiler = a ++ [crearAlquiler(parametros)]
    let listaNuevaBicicleta = cambiarUbicacion b biciObjeto "en transito"
    let listaBicisArchivo = cambiarUbicacion bA biciObjeto "en transito"
    let archivoBicis = bicisAString listaBicisArchivo ""
    escribirNuevosDatos rutaB archivoBicis
    putStr ("\nEl codigo de su alquiler es: " ++ show codigoAlquiler ++ "\n")
    return (p,listaNuevaBicicleta,u,listaNuevaAlquiler, listaBicisArchivo, f)

-- Muestra en pantalla los datos de una factura
--E: una lista con las facturas
--S: N/A
mostrarFactura f = do
    putStr "\nIngrese el codigo de la factura: "
    cod <- getLine
    let codigo = (read cod :: Integer)
    show1Factura f codigo nombreEmpresa sitioWeb contacto
    return ()

--Muestra en pantalla los parqueos
--E: una lista con parqueos y otra con bicicletas
--S: N/A
--Funcionamiento: pide al usuario la provincia y muestra los parqueos de esa provincia
cargarParqueos p b = do
    putStr "\nAL. Alajuela\n"
    putStr "SJ. San Jose\n"
    putStr "HE. Heredia\n"
    putStr "CA. Cartago\n"
    putStr "PU. Puntarenas\n"
    putStr "GU. Guanacaste\n"
    putStr "LI. Limon\n"
    putStr "TP. Todas las provincias\n"
    putStr "Indique la provincia: "
    prov <- getLine
    showParqueos p b prov
    
--Muestra en pantalla las bicicletas
--E: una lista con parqueos y otra con bicicletas
--S: N/A
--Funcionamiento: pide al usuario el nombre del parqueo y muestra las bicicletas de esa provincia
cargarBicicletas p b = do
    putStr "\n'#': Todas las bicicletas del sistema\n"
    putStr "'transito': Todas las bicicletas en transito\n"
    putStr "Tambien puede escribir el nombre de un parqueo\n"
    putStr "Escriba su eleccion: "
    nombreParqueo <- getLine
    putStr "\n"
    if nombreParqueo == "#" then
        showBicicletas b
    else
        if nombreParqueo == "transito" then
            showBicisXParqueo b "en transito"

        else
            show1Parqueo p b nombreParqueo

--Muestra en pantalla los usuarios
--E: una lista con usuarios y otra con los alquileres
--S: N/A
--Funcionamiento: pide al usuario una cedula y muestra los datos del usuario y sus alquileres
cargarUsuarios u a = do
    putStr "\nIngrese '#' para ver la informacion de todos los clientes\n"
    putStr "Ingrese una cedula para ver historial de alquileres de un cliente\n"
    putStr "Eleccion: "
    opcion <- getLine
    putStr "\n"
    if opcion == "#" then
        showUsuarios u
    else
        do
        let cedula = (read opcion :: Integer)
        show1Usuario u cedula
        showAlquileresXUsuario a cedula
    
--Menu de estadisticas
--E: recibe listas con parqueos, bicicletas, usuarios, alquileeres y facturas
--S: N/A, no retorna nada pues el menu de estadisticas no modifica ningun valor de las listas
menuEstadisticas (p, b, u, a, f) =
    do
        putStr "\nMenu Estadisticas\n"
        putStr "1. Top 5 usuarios con mas viajes\n"
        putStr "2. Top 5 parqueos con mas viajes\n"
        putStr "3. Top 3 bicicletas con mas kilometros recorridos\n"
        putStr "4. Resumen\n"
        putStr "5. Volver\n"
        putStr "Ingrese su eleccion: "
        opcion <- getLine
        let opcionInt = (read opcion :: Integer)
        putStr "\n"

        case opcionInt of
            1 -> do
                topUsuarios u a
                menuEstadisticas (p, b, u, a, f)
            2 -> do
                topParqueos p a
                menuEstadisticas (p, b, u, a, f)
            3 -> do
                topBicicletas b a p
                menuEstadisticas (p, b, u, a, f)
            4 -> do
                resumenEstadisticas b p a f
                menuEstadisticas (p, b, u, a, f)
            5 -> return()


menuGeneral (p, b, u, a, rutaB, bA, f) =
    do
        putStr "\nMenu General\n"
        putStr "1. Consultar bicicletas\n"
        putStr "2. Alquilar\n"
        putStr "3. Facturar\n"
        putStr "4. Consulta de factura\n"
        putStr "5. Volver\n"
        putStr "Indique la opcion: "
        tempOpcion <- getLine
        let opcion = (read tempOpcion :: Integer)
        putStr "\n" 
        case opcion of
            1 -> do
                parqueoMasCercanoAux p b
                menuGeneral (p, b, u, a, rutaB, bA, f)
            2 -> do
                tupla <- alquilarBici p b u a rutaB bA f
                menuGeneral (p, getBicis tupla, u, getAlquileres tupla, rutaB, getBicisArchivo tupla, f)
            3 -> do
                tupla <- facturarAlquiler p b u a bA f rutaB
                menuGeneral (p, getBicis tupla, u, getAlquileres tupla, rutaB, getBicisArchivo tupla, getFacturas tupla)
            4 -> do
                mostrarFactura f 
                menuGeneral (p, b, u, a, rutaB, bA, f)
            5 -> 
                return(p, b, u, a, bA, f)
            6 -> do
                showFacturas f
                menuGeneral (p, b, u, a, rutaB, bA, f)

menuOperativo (p, b, u, a, f) =
    do
        putStr "\nMenu Operativo\n"
        putStr "1. Mostrar parqueos\n"
        putStr "2. Mostrar bicicletas\n"
        putStr "3. Mostrar usuarios\n"
        putStr "4. Estadisticas\n"
        putStr "5. Volver\n"
        putStr "Indique la opcion: "
        tempOpcion <- getLine
        let opcion = (read tempOpcion :: Integer)

        case opcion of
            1 -> do
                cargarParqueos p b
                menuOperativo (p, b, u, a, f)
            2 -> do
                cargarBicicletas p b
                menuOperativo (p, b, u, a, f)
            3 -> do
                cargarUsuarios u a
                menuOperativo (p, b, u, a, f)
            4 -> do
                menuEstadisticas (p, b, u, a, f)
                menuOperativo (p, b, u, a, f)
            5 -> return ()
            6 -> do
                showAlquileres a
                menuOperativo (p, b, u, a, f)



menuAux (p, b, u, a, rutaB, bA, f) = 
    do
        putStr "\nMenu Principal\n"
        putStr "1. Opciones operativas\n"
        putStr "2. Opciones generales\n"
        putStr "3. Salir\n"
        putStr "Indique la opcion: "
        tempOpcion <- getLine
        let opcion = (read tempOpcion :: Integer)

        case opcion of
            1 -> do 
                putStr "\nUsuario: "
                usuario <- getLine
                if usuario == "admin" then
                    menuOperativo (p, b, u, a, f)
                else
                    putStr "\nEl usuario que ingreso es incorrecto\n"
                menuAux(p, b, u, a, rutaB, bA, f)
            2 -> do
                tupla <- menuGeneral (p, b, u, a, rutaB, bA, f)
                menuAux(getParqueos tupla, getBicis tupla, getUsuarios tupla, getAlquileres tupla, rutaB, getBicisArchivo tupla, getFacturas tupla)
            3 -> return ()

getParqueos(p, _, _, _, _, _) = p
getBicis(_, b, _, _, _, _) = b
getUsuarios(_, _, u, _, _, _) = u
getAlquileres(_, _, _, a, _, _) = a
getBicisArchivo (_, _, _, _, bA, _) = bA
getFacturas (_, _, _, _, _, f) = f

main = do
    putStr ("Indique la ruta de los parqueos: ")
    ruta <- getLine
    parqueos <- leerArchivoParqueos ruta

    putStr ("Indique la ruta de los usuarios: ")
    ruta <- getLine
    usuarios <- leerArchivoUsuarios ruta

    putStr ("Indique la ruta de las bicicletas: ")
    rutaB <- getLine
    bicicletasArchivo <- leerArchivoBicicletas rutaB
    let cletas = existeParqueo bicicletasArchivo parqueos
    
    alquileres <- leerArchivoAlquileres "al.txt"

    facturas <- leerArchivoFacturas "fa.txt"

    temp <- menuAux (parqueos, cletas, usuarios, alquileres, rutaB, bicicletasArchivo, facturas)
    return temp
