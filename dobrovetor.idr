import Data.String

main : IO ()
main = do
  putStrLn "Digite números separados por espaço:"
  linha <- getLine

  let partes = words linha
  let nums = map (\x => the Int (cast x)) partes
  let dobrados = map (\x => x * 2) nums

  putStrLn ("Dobrado: " ++ show dobrados)
