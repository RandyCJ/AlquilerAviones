module CargaUsuarios where

-- Estructura Usuarios
type Cedula = Integer
type NombreUsuario = String
data Usuario = Usuario Cedula NombreUsuario;

-- Constructor usuario
crearUsuario(elemento) = Usuario (read (elemento!!0) :: Integer) (elemento!!1)
getCedula (Usuario cedula _) = cedula;
getNombreUsuario (Usuario _ nombre) = nombre;

--Muestra Usuarios
showUsuario :: Usuario -> [Char]
showUsuario usuario =
    let
        cedula = getCedula(usuario)
        nombre = getNombreUsuario(usuario)
    in
        "Cedula: " ++ show cedula ++ ", Nombre: " ++ nombre


showUsuarios :: [Usuario] -> IO ()
showUsuarios [] = return()
showUsuarios listaUsuarios =
    do
        print(showUsuario (head listaUsuarios))
        showUsuarios (tail listaUsuarios)

separaPorComas :: ([Char], [Char]) -> [[Char]]
separaPorComas (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
            [temp] ++ separaPorComas ((tail cadena), "")
        else
            separaPorComas ((tail cadena), temp ++ [(head cadena)])

separaElementos :: [[Char]] -> [Usuario]
separaElementos lista =
    if null(lista) then []
    else
        [crearUsuario(separaPorComas((head lista), ""))] ++ separaElementos (tail lista)

leerArchivoUsuarios :: FilePath -> IO [Usuario]
leerArchivoUsuarios archivo = do
    contenido <- readFile archivo
    let usuarios = separaElementos (lines contenido)
    return usuarios
