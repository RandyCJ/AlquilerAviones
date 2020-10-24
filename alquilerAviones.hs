import System.IO
import CargaParqueos
import CargaBicicletas
import CargaUsuarios

cargarParqueos p = do
    showParqueos p

cargarBicicletas b = do
    showBicicletas b

cargarUsuarios u = do
    showUsuarios u

menuAux (opcion, p, b, u) =
    if opcion == 6 then
        print("Adios")
    else
        do
            case opcion of
                -1 -> print()
                1 -> cargarParqueos p
                2 -> cargarBicicletas b
                3 -> cargarUsuarios u

            print("Menu Principal")
            print("1. Mostrar parqueos")
            print("2. Mostrar bicicletas")
            print("3. Mostrar usuarios")
            print("6. Salir")
            putStr "Indique la opcion: "
            tempOpcion <- getLine
            let opcion = (read tempOpcion :: Integer)
            menuAux (opcion, p, b, u)


menu = do
    putStr ("Indique la ruta de los parqueos: ")
    ruta <- getLine
    parqueos <- leerArchivoParqueos ruta

    putStr ("Indique la ruta de las bicicletas: ")
    ruta <- getLine
    bicicletas <- leerArchivoBicicletas ruta

    putStr ("Indique la ruta de los usuarios: ")
    ruta <- getLine
    usuarios <- leerArchivoUsuarios ruta

    temp <- menuAux (-1, parqueos, bicicletas, usuarios)
    return temp

