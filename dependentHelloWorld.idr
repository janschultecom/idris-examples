data EqStr : (s1:String) -> (s2:String) -> Type where
  Say : (s:String) -> EqStr s s 

greet : EqStr "Hello World!" "Hello World!" -> IO ()
greet _ = printLn "Hello World!"

main : IO ()
main =  greet $ Say "Hello World!"

