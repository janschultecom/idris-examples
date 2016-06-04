import Data.Vect


anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["Rod", "Jane", "Freddy"])

read_vect : IO (len ** Vect len String)
read_vect = do x <- getLine
               if (x == "")
                  then pure (_ ** [])
                  else do (_ ** xs) <- read_vect
                          pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- read_vect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- read_vect
               case exactLength len1 vec2 of
                    Nothing => putStr "Wrong sizes"
                    Just vec2' => putStr (show (zip vec1 vec2'))
