data Sentence : (s:String) -> Type where
  Phrase : (s:String) -> Sentence s 

say : Sentence "Hello World!" -> IO ()
say {s} _ = printLn s

main : IO ()
main = say ( Phrase "Hello World!" )

