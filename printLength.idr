module printtest

printLength : IO ()
printLength = getLine >>= (\x => putStrLn (show (Strings.length x)))

printLarger: IO ()
printLarger =
  do
    first <- getLine
    second <- getLine
    let larger:Nat = case compare (length first) (length second) of
             GT => length first
             EQ => length first
             LT => length second
    putStrLn (show larger)

printLarger2: IO ()
printLarger2 = getLine >>= \first => 
               getLine >>= \second => 
                           let larger:Nat = case compare (length first) (length second) of
                                     GT => length first
                                     EQ => length first 
                                     LT => length second in
                            putStrLn (show larger)
