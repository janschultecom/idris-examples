module Main
import Data.Vect


infixr 5 .+.
data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore  where
  constructor MkData
  schema:Schema
  size:Nat
  items:Vect size (SchemaType schema)


addToStore : (store:DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newitem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newitem]
    addToData (x :: xs) = newitem :: x :: xs

sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp = let val = cast inp in
                        if val < 0
                           then Nothing
                        else let newVal = tot + val in
                            Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Size : Command schema
  --Search : String -> Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input) where
  getQuoted : List Char -> Maybe (String, String)
  getQuoted ('"' :: xs)  = case span (/= '"') xs of
                                (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                _ => Nothing
  getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of 
                             ("", rest) => Nothing
                             (num,rest) => Just (cast num, ltrim rest) 
parsePrefix (leftSchema .+. rightSchema) input = case parsePrefix leftSchema input of 
                                                      Nothing => Nothing
                                                      Just (l_val, input') => case parsePrefix rightSchema input' of 
                                                                                   Nothing => Nothing
                                                                                   Just (r_val, input'') => Just ((l_val,r_val), input'')

parseBySchema : (schema:Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                   Just (res,"") => Just res
                                   Just _ => Nothing
                                   Nothing => Nothing

parseCommand : (schema:Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      Just restok => Just (Add restok) 
--parseCommand schema "search" str = Just (Search str)
parseCommand schema "get" val = case all isDigit (unpack val) of
                            False => Nothing
                            True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "size" "" = Just Size
parseCommand _ _ _ = Nothing

parse : (schema:Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = show item 
display {schema = (x .+. y)} (left, right) = display left ++ ", " ++ display right

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                        case integerToFin pos (size store) of
                             Nothing => Just ("Out of range\n", store)
                             Just id => Just (display (index id store_items) ++ "\n", store)

{-searchEntryHelper : (searchStr : String) -> (store : Vect n String) -> (currentResults : List (Nat,String)) -> List (Nat, String)
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
-}

processCmd : (store:DataStore) -> (command : Command (schema store)) -> Maybe (String, DataStore)
processCmd store (Add value) = Just ("ID " ++ show (size store) ++ "\n", addToStore store value)
processCmd store (Get pos)  = getEntry pos store
processCmd store Quit = Nothing
processCmd store Size = Just ("Size " ++ show (size store) ++ "\n", store)
--processCmd store (Search value) = searchEntry value store

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                             Just cmd => processCmd store cmd --cmd store
                             Nothing => Just ("Invalid commnad\n", store)

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
