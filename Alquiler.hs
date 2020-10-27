module Alquiler where
import CargaUsuarios

verificarCedula :: [Usuario] -> Integer -> Integer
verificarCedula [] i = 1
verificarCedula lU cedula = do
    let cedulaA = getCedula (head lU)
    if cedulaA == cedula then
        0
    else
        verificarCedula (tail lU) cedula