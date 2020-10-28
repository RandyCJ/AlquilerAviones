module Estadisticas where
import CargaUsuarios
import CargaParqueos
-- TOP USUARIOS
topUsuarios u a = do
    listaTOP <- obtenerTOPUsuarios u a []
    return ()

insertarUsuario :: [[String]] -> [String] -> [[String]]
insertarUsuario lTOP datos = do
    if null(lTOP) then
        [datos]
    else
        if (read (datos!!1) :: Integer) > (read ((head lTOP)!!1) :: Integer) then
            [datos] ++ lTOP
        else
            [(head lTOP)] ++ insertarUsuario (tail lTOP) datos


obtenerTOPUsuarios :: [Usuario] -> [Alquiler] -> [[String]] -> IO [[String]]
obtenerTOPUsuarios [] lA lTOP = return lTOP
obtenerTOPUsuarios lU lA lTOP = do
    let largoLista = length lTOP
    let cantidad = obtenerAlquileresXUsuario (head lU) lA 0
    let nombreUsurio = getNombreUsuario (head lU)
    if largoLista == 0 then
        do
        obtenerTOPUsuarios (tail lU) lA [[nombreUsuario, show cantidad]]
    else
        do
        let listaTOP = insertarUsuario lTOP [nombreUsuario, show cantidad]
        obtenerTOPUsuarios (tail lU) lA listaTOP
        

obtenerAlquileresXUsuario :: Usuario -> [Alquiler] -> Integer -> Integer
obtenerAlquileresXUsuario usuario [] cantidad = cantidad
obtenerAlquileresXUsuario usuario lA cantidad = do
    let 
        cedula = getCedula usuario
        cedulaAlquiler = getCedulaAlquiler (head lA)

    if cedula == cedulaAlquiler then
        obtenerAlquileresXUsuario usuario (tail lA) (cantidad + 1)
    else
        obtenerAlquileresXUsuario usuario (tail lA) cantidad

-- TOP PARQUEOS
topParqueos p a = do
    listaTOP <- obtenerTOPParqueos p a []
    return ()

obtenerTOPParqueos :: [Parqueo] -> [Alquiler] -> [[String]] -> IO [[String]]
obtenerTOPParqueos [] _ lTOP = return lTOP
obtenerTOPParqueos lP lA lTOP = do
    let largoLista = length lTOP
    let cantidad = obtenerViajesXParqueo (head lP) lA 0
    let nombreParqueo = getNombreParqueo (head lP)
    if largoLista == 0 then
        do
        obtenerTOPParqueos (tail lP) lA [[nombreParqueo, show cantidad]]
    else
        do
        let listaTOP = insertarUsuario lTOP [nombreParqueo, show cantidad]
        obtenerTOPParqueos (tail lP) lA listaTOP
        

obtenerViajesXParqueo :: Parqueo -> [Alquiler] -> Integer -> Integer
obtenerViajesXParqueo p [] cantidad = cantidad
obtenerViajesXParqueo parqueo lA cantidad = do
    let 
        nombre = getNombreParqueo parqueo
        parqueoSalida = getParqueoSalida (head lA)
        parqueoLlegada = getParqueoLlegada (head lA)

    if nombre == parqueoSalida then
        obtenerViajesXParqueo parqueo (tail lA) (cantidad + 1)
    else
        if nombre == parqueoLlegada then
            obtenerViajesXParqueo parqueo (tail lA) (cantidad + 1)
        else
            obtenerViajesXParqueo parqueo (tail lA) cantidad