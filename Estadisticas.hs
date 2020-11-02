module Estadisticas where
import CargaUsuarios
import CargaParqueos
import CargaBicicletas
import FuncionesGenerales
import Alquileres
import Facturas

--Imprime los tops
--E: una lista con listas de strings ordenadas en orden de mayor a menor
--   el numero maximo de elementos a imprimir, un contador
--S: N/A
imprimirTOPS :: [[String]] -> Integer -> Integer -> IO ()
imprimirTOPS [] c i = return ()
imprimirTOPS lTOP maximo contador = do
    if contador > maximo then
        return ()
    else
        do
        let actual = (head lTOP)
        putStr ("\t" ++ (actual!!0) ++ "||" ++ (actual!!1) ++ "\n")
        imprimirTOPS (tail lTOP) maximo (contador +1)

--Inserta en una lista de listas de strings un valor en su espacio en una lista descendente
--E: la lista con listas de strings, el string a insertar ordenadamente
--S: La lista de listas de string ya ordenada con el nuevo insertado
insertarOrdenado :: [[String]] -> [String] -> [[String]]
insertarOrdenado lTOP datos = do
    if null(lTOP) then
        [datos]
    else
        if (read (datos!!1) :: Integer) > (read ((head lTOP)!!1) :: Integer) then
            [datos] ++ lTOP
        else
            [(head lTOP)] ++ insertarOrdenado (tail lTOP) datos

--Inserta en una lista de listas de strings un valor en su espacio en una lista descendente
--E: la lista con listas de strings, el string a insertar ordenadamente
--S: La lista de listas de string ya ordenada con el nuevo insertado
insertarOrdenadoBici :: [[String]] -> [String] -> [[String]]
insertarOrdenadoBici lTOP datos = do
    if null(lTOP) then
        [datos]
    else
        if (read (datos!!1) :: Float) > (read ((head lTOP)!!1) :: Float) then
            [datos] ++ lTOP
        else
            [(head lTOP)] ++ insertarOrdenadoBici (tail lTOP) datos
            

-- TOP USUARIOS
topUsuarios u a = do
    listaTOP <- obtenerTOPUsuarios u a []
    putStr "\tNombre||Total de alquileres\n\n"
    imprimirTOPS listaTOP 5 1
    return ()

--Obtiene una lista de listas strigs ordenadas descendentemente
--E: lista de usuarios, lista de alquileres, lista vacia donde se guardaran los valores
--S: la lista ya ordenada descendentemente
obtenerTOPUsuarios :: [Usuario] -> [Alquiler] -> [[String]] -> IO [[String]]
obtenerTOPUsuarios [] lA lTOP = return lTOP
obtenerTOPUsuarios lU lA lTOP = do
    let largoLista = length lTOP
    let cantidad = obtenerAlquileresXUsuario (head lU) lA 0
    let nombreUsuario = getNombreUsuario (head lU)
    if largoLista == 0 then
        do
        obtenerTOPUsuarios (tail lU) lA [[nombreUsuario, show cantidad]]
    else
        do
        let listaTOP = insertarOrdenado lTOP [nombreUsuario, show cantidad]
        obtenerTOPUsuarios (tail lU) lA listaTOP        

--Obtiene la cantidad de alquileres de cada usuario
--E: un usuario, lista de alquileres, un entero 0 que se ira sumando cuando encuente un alquiler que le corresponda al usuario
--S: la cantidad de alquileres de esa persona
obtenerAlquileresXUsuario :: Usuario -> [Alquiler] -> Integer -> Integer
obtenerAlquileresXUsuario usuario [] cantidad = cantidad
obtenerAlquileresXUsuario usuario lA cantidad = do
    let 
        cedula = getCedula usuario
        cedulaAlquiler = getCedulaUsuario (head lA)
        
    if cedula == cedulaAlquiler then
        obtenerAlquileresXUsuario usuario (tail lA) (cantidad + 1)
    else
        obtenerAlquileresXUsuario usuario (tail lA) cantidad

-- TOP PARQUEOS
topParqueos p a = do
    listaTOP <- obtenerTOPParqueos p a []
    putStr "\tNombre||Total de viajes\n\n"
    imprimirTOPS listaTOP 5 1
    return ()

--Obtiene una lista de listas strigs ordenadas descendentemente
--E: lista de parqueos, lista de alquileres, lista vacia donde se guardaran los valores
--S: la lista ya ordenada descendentemente
obtenerTOPParqueos :: [Parqueo] -> [Alquiler] -> [[String]] -> IO [[String]]
obtenerTOPParqueos [] lA lTOP = return lTOP
obtenerTOPParqueos lP lA lTOP = do
    let largoLista = length lTOP
    let cantidad = obtenerViajesXParqueo (head lP) lA 0
    let nombreParqueo = getNombreParqueo (head lP)
    if largoLista == 0 then
        do
        obtenerTOPParqueos (tail lP) lA [[nombreParqueo, show cantidad]]
    else
        do
        let listaTOP = insertarOrdenado lTOP [nombreParqueo, show cantidad]
        obtenerTOPParqueos (tail lP) lA listaTOP
        
--Obtiene la viajes de cada parqueo
--E: un parqueo, lista de alquileres, un entero 0 que se ira sumando cuando encuente un viaje que corresponda al parqueo
--S: la cantidad de viajes de ese parqueo
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

--TOP Bicicletas
topBicicletas b a p = do
    listaTOP <- obtenerTOPBicicletas b a p []
    putStr "\tCodigo||Total de kilometros\n\n"
    imprimirTOPS listaTOP 3 1
    return ()

--Obtiene una lista de listas strigs ordenadas descendentemente
--E: lista de bicicletas, lista de alquileres, lista de parqueos, lista vacia donde se guardaran los valores
--S: la lista ya ordenada descendentemente
obtenerTOPBicicletas :: [Bicicleta] -> [Alquiler] -> [Parqueo] -> [[String]] -> IO [[String]]
obtenerTOPBicicletas [] lA lP lTOP = return lTOP
obtenerTOPBicicletas lB lA lP lTOP = do
    let largoLista = length lTOP
    let kilometros = obtenerKilometrosXBici (head lB) lP lA 0
    let codigoBici = getCodigo (head lB)

    if largoLista == 0 then
        obtenerTOPBicicletas (tail lB) lA lP [[codigoBici, show kilometros]]
    else
        do
        let listaTOP = insertarOrdenadoBici lTOP [codigoBici, show kilometros]
        obtenerTOPBicicletas (tail lB) lA lP listaTOP

--Obtiene los kilometros recorridos por cada bicicleta
--E: una bicicleta, lista de parqueos, lista de alquileres, un entero 0 que se ira sumando cuando encuente un viaje que corresponda al parqueo
--S: la cantidad de kilometros de esa bici
obtenerKilometrosXBici :: Bicicleta -> [Parqueo] -> [Alquiler] -> Float -> Float
obtenerKilometrosXBici b lP [] kilometrosTotales = kilometrosTotales
obtenerKilometrosXBici bici lP lA kilometrosTotales = do 
    let codigoBici = getCodigo bici
    let codigoBiciAlquiler = getCodigoBici (head lA)
    let estadoAlquiler = getEstado (head lA)

    if estadoAlquiler == "activo" then
        obtenerKilometrosXBici bici lP (tail lA) kilometrosTotales
    else
        if codigoBici == codigoBiciAlquiler then
            do
                let parqueoLlegada = getParqueo (getParqueoLlegada (head lA)) lP
                let parqueoSalida = getParqueo (getParqueoSalida (head lA)) lP
                let xPL = getUbicacionX (parqueoLlegada)
                let yPL = getUbicacionY (parqueoLlegada)
                let xPS = getUbicacionX (parqueoSalida)
                let yPS = getUbicacionY (parqueoSalida)
                let distancia = obtenerDistancia xPL yPL xPS yPS
                obtenerKilometrosXBici bici lP (tail lA) (kilometrosTotales + distancia)
        else
            obtenerKilometrosXBici bici lP (tail lA) kilometrosTotales

--Resumen

--obtiene la suma de todos los kilometros de todas las bicis
--E: lista de bicicletas, lista de parqueos, lista de alquileres, un 0 que ira sumando los kilometros
totalKilometros :: [Bicicleta] -> [Parqueo] -> [Alquiler] -> Float -> Float
totalKilometros [] lP lA total = total 
totalKilometros lB lP lA total = do 
        let kilometros = obtenerKilometrosXBici (head lB) lP lA 0
        totalKilometros (tail lB) lP  lA  (total+kilometros)

--Imprime en pantalla las estadisticas
--E: lista de bicicletas, lista de parqueos, lista de alquileres, lista de facturas
--S: S/A
resumenEstadisticas :: [Bicicleta] -> [Parqueo] -> [Alquiler] -> [Factura] -> IO ()
resumenEstadisticas lB lP lA lF = do
    putStr "\nResumen general\n"

    let totalViajes = length lA
    putStr ("\nTotal de viajes: " ++ show totalViajes)

    let kilometrosTotales = totalKilometros lB lP lA 0
    putStr ("\nKilometros totales: " ++ show kilometrosTotales)

    let totalFacturas = length lF
    putStr ("\nTotal de facturas generadas: " ++ show totalFacturas ++ "\n")