greet : (=) "Hello World!" "Hello World!" -> IO ()
greet _ = printLn "Hello World!"

main : IO ()
main = greet $ Refl { x = "Hello World!" } 


