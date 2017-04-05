greet : (s:String) -> { auto ok : (=) s "Hello World!"} -> IO ()
greet s = printLn s

main : IO ()
main = greet "Hello World!"


