module Alquileres where
import CargaUsuarios
import FuncionesGenerales

verificarCedula :: [Usuario] -> Integer -> Integer
verificarCedula [] i = 1
verificarCedula lU cedula = do
    let cedulaA = getCedula (head lU)
    if cedulaA == cedula then
        0
    else
        verificarCedula (tail lU) cedula



-- Estructura Alquiler
type Identificador = Integer
type Estado = String
type CedulaUsuario = Integer
type ParqueoSalida = String
type ParqueoLlegada = String
type CodigoBicicleta = String
type TipoBici = String
data Alquiler = Alquiler Identificador Estado CedulaUsuario ParqueoSalida ParqueoLlegada CodigoBicicleta TipoBici;

-- Constructor Alquileres
crearAlquiler(elemento) = Alquiler (read (elemento!!0) :: Integer) (elemento!!1) (read (elemento!!2) :: Integer) (elemento!!3) (elemento!!4) (elemento!!5) (elemento!!6)
getIdentificador (Alquiler identificador _ _ _ _ _ _ ) = identificador;
getEstado (Alquiler _ estado _ _ _ _ _ ) = estado;
getCedulaUsuario(Alquiler _ _ cedula _ _ _ _ ) = cedula;
getParqueoSalida (Alquiler _ _ _ parqueoS _ _ _ ) = parqueoS;
getParqueoLlegada (Alquiler _ _ _ _ parqueo _ _ ) = parqueo;
getCodigoBici (Alquiler _ _ _ _ _ codigoBici _  ) = codigoBici;
getTipoBici (Alquiler _ _ _ _ _ _ tipoBici ) = tipoBici;


