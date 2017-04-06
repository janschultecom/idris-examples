say : (=) "Hello World!" "Hello World!" -> IO ()
say _ = printLn "Hello World!"

main : IO ()
main = say $ Refl { x = "Hello World!" } 


