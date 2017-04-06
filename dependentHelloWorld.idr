data Sentence : String -> Type where
  Phrase : (s:String) -> Sentence s 

say : Sentence "Hello World!" -> IO ()
say _ = printLn "Hello World!"

main : IO ()
main = say ( Phrase "Hello World!" )

