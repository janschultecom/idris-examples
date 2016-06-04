module Main
import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do Just num1_ok <- readNumber
     Just num2_ok <- readNumber
     pure (Just (num1_ok, num2_ok))

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do putStr ("Guess the number (" ++ show guesses ++ " guesses so far): ")
                          Just startNum <- readNumber | Nothing => do putStrLn "Invalid input"
                                                                      guess target guesses
                          case compare startNum target of
                             LT => do putStrLn "Too low"
                                      guess target (S guesses)
                             EQ => do putStrLn ("You finally got it: " ++ show target ++ " (after " ++ show guesses ++ " tries)")
                                      pure ()
                             GT => do putStrLn "Too high"
                                      guess target (S guesses)

main : IO ()
main = do rand <- time 
          let randy = cast (mod rand 100)
          guess randy 0
