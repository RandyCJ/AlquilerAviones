import System.IO
import CargaParqueos
import CargaUsuarios
import CargaBicicletas
import Alquiler


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
            showBicisXParqueo b "transito"
        else
            show1Parqueo p b nombreParqueo

cargarUsuarios u = do
    putStr "\nIngrese '#' para ver la informacion de todos los clientes\n"
    putStr "Ingrese una cedula para ver historial de alquileres de un cliente\n"
    putStr "Eleccion: "
    opcion <- getLine
    putStr "\n"
    if opcion == "#" then
        showUsuarios u
    else
        do
        let opcionInt = (read opcion :: Integer)
        show1Usuario u opcionInt
    


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

-- Desde esta funcion se piden todos los datos para el alquiler
alquilarBici p b u = do
    cedula <- solicitarCedula u
    putStr ("La cedula es: " ++ show cedula)
    return ()

menuEstadisticas (p, b, u) =
    do
        putStr "\nMenu Estadisticas"
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
                putStr "Se muestra el TOP usuarios\n"
                menuEstadisticas (p, b, u)
            2 -> 
                do
                putStr "Se muestra el TOP parqueos\n"
                menuEstadisticas (p, b, u)
            3 -> do
                putStr "Se muestra el TOP bicicletas\n"
                menuEstadisticas (p, b, u)
            4 -> do
                putStr "Se muestra el resumen\n"
                menuEstadisticas (p, b, u)
            5 -> return()


menuGeneral (p, b, u) =
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
                menuGeneral (p, b, u)
            2 -> do
                alquilarBici p b u
                menuGeneral (p, b, u)
            3 -> do
                putStr "Facturar\n"
                menuGeneral (p, b, u)
            4 -> do
                putStr "Consulta factura\n"
                menuGeneral (p, b, u)
            5 -> 
                return()
                
                
            

menuOperativo (p, b, u) =
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
                menuOperativo (p, b, u)
            2 -> do
                cargarBicicletas p b
                menuOperativo (p, b, u)
            3 -> do
                cargarUsuarios u
                menuOperativo (p, b, u)
            4 -> do
                menuEstadisticas (p, b, u)
                menuOperativo (p, b, u)
            5 -> return ()


menuAux (p, b, u) = 
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
                menuOperativo (p, b, u)
                menuAux (p, b, u)
            2 -> do
                menuGeneral (p, b, u)
                menuAux(p, b, u)
            3 -> return ()
        

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

    temp <- menuAux (parqueos, bicicletas, usuarios)
    return temp

