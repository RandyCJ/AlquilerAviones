import System.IO
import CargaParqueos
import CargaUsuarios
import CargaBicicletas


cargarParqueos p b = do
    print("1. Alajuela")
    print("2. San Jose")
    print("3. Heredia")
    print("4. Cartago")
    print("5. Puntarenas")
    print("6. Guanacaste")
    print("7. Limón")
    print("8. Todas las provincias")
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
    

cargarBicicletas b = do
    showBicicletas b

cargarUsuarios u = do
    showUsuarios u

menuOperativo (p, b, u) =
    do
        print("Menu Principal")
        print("1. Mostrar parqueos")
        print("2. Mostrar bicicletas")
        print("3. Mostrar usuarios")
        print("4. Estadisticas")
        print("5. Volver")
        putStr "Indique la opcion: "
        tempOpcion <- getLine
        let opcion = (read tempOpcion :: Integer)

        case opcion of
            1 -> do
                cargarParqueos p b
                menuOperativo (p, b, u)
            2 -> do
                cargarBicicletas b
                menuOperativo (p, b, u)
            3 -> do
                cargarUsuarios u
                menuOperativo (p, b, u)
            4 -> do
                menuOperativo (p, b, u)
            5 -> return ()


menuAux (p, b, u) = 
    do
        print("Menu Principal")
        print("1. Opciones operativas")
        print("2. Opciones generales")
        print("3. Salir")
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

