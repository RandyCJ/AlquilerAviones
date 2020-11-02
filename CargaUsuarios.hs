module CargaUsuarios where
import FuncionesGenerales

-- Estructura Usuarios
type Cedula = Integer
type NombreUsuario = String
data Usuario = Usuario Cedula NombreUsuario;

-- Constructor usuario
crearUsuario(elemento) = Usuario (read (elemento!!0) :: Integer) (elemento!!1)
getCedula (Usuario cedula _) = cedula;
getNombreUsuario (Usuario _ nombre) = nombre;

--Muestra un usuario
--E: un usuario
--S: N/A
showUsuario :: Usuario -> IO ()
showUsuario usuario =
    let
        cedula = getCedula(usuario)
        nombre = getNombreUsuario(usuario)
    in
        putStr ("Cedula: " ++ show cedula ++ ", Nombre: " ++ nombre ++ "\n")

--Muestra todos los usuario
--E: una lista de usuarios
--S: N/A
showUsuarios :: [Usuario] -> IO ()
showUsuarios [] = return()
showUsuarios listaUsuarios =
    do
        showUsuario (head listaUsuarios)
        showUsuarios (tail listaUsuarios)

--Muestra la info de un usuario 
--E: lista de usuarios, una cedula a verificar que exista
--S: N/A
show1Usuario :: [Usuario] -> Integer -> IO ()
show1Usuario [] i = do
    putStr "\n La cedula que ingreso no se encuentra en el sistema\n"
    return()
show1Usuario lU cedula =
    do
        let cedulaU = getCedula (head lU)
        if cedulaU == cedula then
            do
            showUsuario (head lU)
            putStr "Alquileres\n"
        else
            show1Usuario (tail lU) cedula

--Crea una lista de usuarios
--E: una lista con listas de strings
--S: una lista de usuarios
separaUsuarios :: [[Char]] -> [Usuario]
separaUsuarios lista =
    if null(lista) then []
    else
        [crearUsuario(separaPorComas((head lista), ""))] ++ separaUsuarios (tail lista)

--lee un archivo de usuarios
--E: la ruta del archivo
--S: retorna una lista de usuarios
leerArchivoUsuarios :: FilePath -> IO [Usuario]
leerArchivoUsuarios archivo = do
    contenido <- readFile archivo
    let usuarios = separaUsuarios (lines contenido)
    return usuarios
