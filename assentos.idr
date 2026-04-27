||| Mostra um mapa de assentos e permite reservá-los

import Data.Vect
import Data.Fin
import Data.String

data Assento = Livre | Ocupado

-- Cria vetor de n assentos
Assentos : Nat -> Type
Assentos n = Vect n Assento

-- Função que converte um assento e seu índice em String
mostrarUm : Assento -> Nat -> String
mostrarUm Livre idx   = "[" ++ show idx ++ "]"
mostrarUm Ocupado idx = "[X]"

-- Função recursiva para montar a string de todos os assentos
renderizar : {n : Nat} -> Assentos n -> Nat -> String
renderizar [] _ = ""
renderizar (x :: xs) curr = (mostrarUm x curr) ++ " " ++ renderizar xs (curr + 1)

-- Cria vetor de n assentos e deixa todos livres
criarAssentos : (n : Nat) -> Assentos n
criarAssentos Z = []
criarAssentos (S k) = Livre :: criarAssentos k

-- Reserva um assento de ele estiver livre
reservar : (i : Fin n) -> Assentos n -> Maybe (Assentos n)
reservar i xs =
  case index i xs of
      Livre   => Just (updateAt i (\_ => Ocupado) xs)
      Ocupado => Nothing

-- Converte Int para Fin
intToFin : (n : Nat) -> Integer -> Maybe (Fin n)
intToFin Z _ = Nothing
intToFin (S k) x =
  if x < 0 then Nothing
  else case x of
    0 => Just FZ
    _ => case intToFin k (x - 1) of
      Just f => Just (FS f)
      Nothing => Nothing

-- Loop de Escolha de assentos
loop : (n : Nat) -> Assentos n -> IO ()
loop n assentos = do
  putStrLn ("\nAssentos: " ++ renderizar assentos 0)
  putStrLn "Digite o número do assento (ou -1 para sair):"
  input <- getLine

  case parseInteger input of
    Nothing => do
      putStrLn "Entrada inválida!"
      loop n assentos
    Just idx =>
      if idx == -1 then
        putStrLn "Encerrando..."
      else
        case intToFin n idx of
          Nothing => do
            putStrLn "Índice fora do intervalo!"
            loop n assentos
          Just i =>
            case reservar i assentos of
              Nothing => do
                putStrLn "Assento já está ocupado!"
                loop n assentos
              Just novos => do
                putStrLn "Reserva realizada com sucesso!"
                loop n novos

-- Main
main : IO ()
main = do
  putStrLn "Digite o número de asssentos que existirão:"
  entrada <- getLine
  let n = the Nat (cast entrada)
  loop n (criarAssentos n)