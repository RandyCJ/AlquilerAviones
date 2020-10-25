import System.IO
import CargaParqueos
import CargaUsuarios
import CargaBicicletas


cargarParqueos p b = do
    putStr "1. Alajuela\n"
    putStr "2. San Jose\n"
    putStr "3. Heredia\n"
    putStr "4. Cartago\n"
    putStr "5. Puntarenas\n"
    putStr "6. Guanacaste\n"
    putStr "7. Limon\n"
    putStr "8. Todas las provincias\n"
    putStr "Indique la provincia: "
    prov <- getLine
    let provincia = (read prov :: Integer)

    case provincia of
        1 -> showParqueos p b "AL"
        2 -> showParqueos p b "SJ"
        3 -> showParqueos p b "HE"
        4 -> showParqueos p b "CA"
        5 -> showParqueos p b "PU"
        6 -> showParqueos p b "GU"
        7 -> showParqueos p b "LI"
        8 -> showParqueos p b "TP"
    

cargarBicicletas p b = do
    putStr "'#': Todas las bicicletas del sistema\n"
    putStr "'transito': Todas las bicicletas en transito\n"
    putStr "Tambien puede escribir el nombre de un parqueo\n"
    putStr "Escriba su eleccion: "
    nombreParqueo <- getLine

    if nombreParqueo == "#" then
        showBicicletas b
    else
        if nombreParqueo == "transito" then
            showBicisXParqueo b "transito"
        else
            show1Parqueo p b nombreParqueo

cargarUsuarios u = do
    showUsuarios u

menuOperativo (p, b, u) =
    do
        putStr "Menu Principal\n"
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
                menuOperativo (p, b, u)
            5 -> return ()


menuAux (p, b, u) = 
    do
        putStr "Menu Principal\n"
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
            2 -> return () --menuGeneral (-1, p, b, u)
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

