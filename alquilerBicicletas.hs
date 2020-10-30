import System.IO
import CargaParqueos
import CargaUsuarios
import CargaBicicletas
import Alquileres
import FuncionesGenerales
import Estadisticas
import Data.List



alquilarBici p b u a = do
    cedula <- solicitarCedula u
    parqueoObjeto <- parqueoMasCercanoAux p b
    putStr "Este es el parqueo mas cercano de salida\n"
    putStr "\nIndique el codigo de la bici de su preferencia:"
    bicicleta <- getLine
    let biciObjeto = getBicicleta bicicleta b
    putStr "\nParqueos de llegada:\n"
    showParqueosSOLOS p
    putStr "\nSeleccione el parqueo de llegada (Con el nombre): \n"
    parqueoL <- getLine
    let codigoAlquiler = length a + 1
    let parametros = [show codigoAlquiler, "activo", show cedula, getNombreParqueo parqueoObjeto, parqueoL, bicicleta, getTipo biciObjeto]
    let listaNuevaAlquiler = a ++ [crearAlquiler(parametros)]
    let listaNuevaBicicleta = cambiarUbicacion b biciObjeto

    return (p,listaNuevaBicicleta,u,listaNuevaAlquiler)
   


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
    

menuEstadisticas (p, b, u, a) =
    do
        putStr "\nMenu Estadisticas\n"
        putStr "1. Top 5 usuarios con mas viajes\n"
        putStr "2. Top 5 parqueos con mas viajes\n"
        putStr "3. Top 3 parqueos con mas kilometros recorridos\n"
        putStr "4. Resumen\n"
        putStr "5. Volver\n"
        putStr "Ingrese su eleccion: "
        opcion <- getLine
        let opcionInt = (read opcion :: Integer)
        putStr "\n"

        case opcionInt of
            1 -> do
                topUsuarios u a
                menuEstadisticas (p, b, u, a)
            2 -> do
                topParqueos p a
                menuEstadisticas (p, b, u, a)
            3 -> do
                topBicicletas b a p
                menuEstadisticas (p, b, u, a)
            4 -> do
                putStr "Se muestra el resumen\n"
                menuEstadisticas (p, b, u, a)
            5 -> return()


menuGeneral (p, b, u, a) =
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
                menuGeneral (p, b, u, a)
            2 -> do
                tupla <- alquilarBici p b u a
                menuGeneral (p, getBicis tupla, u, getAlquileres tupla)
            3 -> do
                putStr "Facturar\n"
                menuGeneral (p, b, u, a)
            4 -> do
                putStr "Consulta factura\n"
                menuGeneral (p, b, u, a)
            5 -> 
                return(p, b, u, a)


menuOperativo (p, b, u, a) =
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
                menuOperativo (p, b, u, a)
            2 -> do
                cargarBicicletas p b
                menuOperativo (p, b, u, a)
            3 -> do
                cargarUsuarios u a
                menuOperativo (p, b, u, a)
            4 -> do
                menuEstadisticas (p, b, u, a)
                menuOperativo (p, b, u, a)
            5 -> return (p, b, u, a)
            6 -> do
                showAlquileres a
                menuOperativo (p, b, u, a)



menuAux (p, b, u, a) = 
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
                tupla <- menuOperativo (p, b, u, a)
                menuAux(getParqueos tupla, getBicis tupla, getUsuarios tupla, getAlquileres tupla)
            2 -> do
                tupla <- menuGeneral (p, b, u, a)
                menuAux(getParqueos tupla, getBicis tupla, getUsuarios tupla, getAlquileres tupla)
            3 -> return ()


getBicis(_, b, _, _) = b
getAlquileres(_, _, _, a) = a
getUsuarios(_, _, u, _) = u
getParqueos(p, _, _, _) = p

main = do
    putStr ("Indique la ruta de los parqueos: ")
    ruta <- getLine
    parqueos <- leerArchivoParqueos ruta

    putStr ("Indique la ruta de las bicicletas: ")
    ruta <- getLine
    bicicletas <- leerArchivoBicicletas ruta

    putStr ("Indique la ruta de los usuarios: ")
    ruta <- getLine
    usuarios <- leerArchivoUsuarios ruta
    
    alquileres <- leerArchivoAlquileres "al.txt"

    temp <- menuAux (parqueos, bicicletas, usuarios, alquileres)
    return temp


escribirArchivo :: String -> String -> IO ()
escribirArchivo ruta lista = do
    writeFile ruta lista
    return ()
--appendFile ruta "\n"
