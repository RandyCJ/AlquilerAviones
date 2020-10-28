module Estadisticas where
import CargaUsuarios

topUsuarios u a = do
    listaTOP <- obtenerTOP u a []
    return ()

insertarUsuario :: [[String]] -> [String] -> Integer -> [[String]]
insertarUsuario lTOP datos contador = do
    if null(lTOP) then
        [datos]
    else
        if (read (datos!!1) :: Integer) > (read ((head lTOP)!!1) :: Integer) then
            do
            [datos] ++ lTOP
        else
            [(head lTOP)] ++ insertarUsuario (tail lTOP) datos (contador + 1)


obtenerTOP :: [Usuario] -> [Alquiler] -> [[String]] -> IO [[String]]
obtenerTOP [] _ lTOP = lTOP
obtenerTOP lU lA lTOP = do
    let largoLista = length lTOP
    let cantidad = obtenerAlquileresXUsuario (head lU) lA 0
    if largoLista == 0 then
        do
        obtenerTOP (tail lU) lA [[getNombreUsuario (head lU), show cantidad]]
    else
        do
        let listaTOP = insertarUsuario lTOP [getNombreUsuario (head lU), cantidad] 0
        obtenerTOP (tail lU) lA listaTOP
        

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