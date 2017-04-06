module Main
import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = newitem :: x :: xs

sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp = let val = cast inp in
                        if val < 0
                           then Nothing
                        else let newVal = tot + val in
                            Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "search" str = Just (Search str)
parseCommand "get" val = case all isDigit (unpack val) of
                            False => Nothing
                            True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (index id store_items ++ "\n", store)

searchEntryHelper : (searchStr : String) -> (store : Vect n String) -> (currentResults : List (Nat,String)) -> List (Nat, String)
searchEntryHelper searchStr [] res = res
searchEntryHelper {n = S k} searchStr (x :: xs) res =
  let newResults =
    case Strings.isInfixOf searchStr x of
         True => (k,x) :: res
         False => res
  in searchEntryHelper searchStr xs newResults

searchEntry : (value : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntry value store @ (MkData size items) = let search_result = searchEntryHelper value items Nil in
                                            Just ("Search results " ++ show (search_result) ++ "\n", store)

processCmd : (command : Command) -> (store : DataStore) -> Maybe (String, DataStore)
processCmd (Add value) store = Just ("ID " ++ show (size store) ++ "\n", addToStore store value)
processCmd (Get pos) store = getEntry pos store
processCmd Quit store = Nothing
processCmd Size store = Just ("Size " ++ show (size store) ++ "\n", store)
processCmd (Search value) store = searchEntry value store

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                              Just cmd => processCmd cmd store
                              Nothing => Just ("Invalid commnad\n", store)

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
