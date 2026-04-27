||| Calcula o fatorial de um número

fatorial : Nat -> Nat
fatorial Z = 1
fatorial (S k) = (S k) * fatorial k

main : IO ()
main = do
  putStrLn "Digite um número:"
  entrada <- getLine
  let n = the Nat (cast entrada)

  putStrLn ("Fatorial: " ++ show (fatorial n))
