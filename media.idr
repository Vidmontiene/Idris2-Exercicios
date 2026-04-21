main : IO ()
main = do
  putStrLn "Nota 1: "
  nota1 <- getLine
  let n1 = the Double (cast nota1)

  putStrLn "Nota 2: "
  nota2 <- getLine
  let n2 = the Double (cast nota2)

  putStrLn ("Media: " ++ show ((n1 + n2) / 2.0))
