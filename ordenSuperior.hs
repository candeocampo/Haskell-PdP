data Cliente = Cliente{
    nombre :: String,
    deuda :: Float,
    facturas :: [Float]
} deriving (Show)

--Lista de Clientes
clientes = [ 
    Cliente "Biasutto" 6000 [4000, 5000],
    Cliente "Colombatti" 15000 [30000],
    Cliente "Marabotto" 200 [500000, 140000]
 ]

--EJ 1: Clientes que nos deben más de 10mil

clientesQueDeben :: Float -> [Cliente] -> [Cliente]
clientesQueDeben plata [] = []
clientesQueDeben plata (cliente:clientes)
    | ((>plata).deuda) cliente = cliente:clientesQueDeben plata clientes
    | otherwise = clientesQueDeben plata clientes