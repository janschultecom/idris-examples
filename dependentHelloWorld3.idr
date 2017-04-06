say : (s:String) -> { auto ok : (=) s "Hello World!"} -> IO ()
say s = printLn s

main : IO ()
main = say "Hello World!"


