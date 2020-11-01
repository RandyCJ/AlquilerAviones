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

--Muestra Usuarios
showUsuario :: Usuario -> IO ()
showUsuario usuario =
    let
        cedula = getCedula(usuario)
        nombre = getNombreUsuario(usuario)
    in
        putStr ("Cedula: " ++ show cedula ++ ", Nombre: " ++ nombre ++ "\n")


showUsuarios :: [Usuario] -> IO ()
showUsuarios [] = return()
showUsuarios listaUsuarios =
    do
        showUsuario (head listaUsuarios)
        showUsuarios (tail listaUsuarios)

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

separaUsuarios :: [[Char]] -> [Usuario]
separaUsuarios lista =
    if null(lista) then []
    else
        [crearUsuario(separaPorComas((head lista), ""))] ++ separaUsuarios (tail lista)

leerArchivoUsuarios :: FilePath -> IO [Usuario]
leerArchivoUsuarios archivo = do
    contenido <- readFile archivo
    let usuarios = separaUsuarios (lines contenido)
    return usuarios
