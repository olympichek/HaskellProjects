import Text.Read ( readMaybe )
import Data.List.Split ( splitOn )

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
    deriving Show
    
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving Show

splitIntoLines :: String -> Either Error [String]
splitIntoLines str = let strs = lines str in
    if length strs >= 3
        then Right strs
        else Left IncompleteDataError
    
getFirstName :: [String] -> Either Error String
getFirstName ["firstName", firstName] = Right firstName
getFirstName _ = Left ParsingError
    
getLastName :: [String] -> Either Error String
getLastName ["lastName", lastName] = Right lastName
getLastName _ = Left ParsingError

getAge :: [String] -> Either Error Int
getAge ["age", age] =
    case readMaybe age of
        Just age' -> Right age'
        Nothing   -> Left $ IncorrectDataError age
getAge _ = Left ParsingError

splitAndTrim :: String -> [String]
splitAndTrim = map (trimLeft . trimRight) . splitOn "="

trimLeft :: String -> String
trimLeft = dropWhile (== ' ')

trimRight :: String -> String
trimRight = reverse . dropWhile (== ' ') . reverse

parsePerson :: String -> Either Error Person
parsePerson str = do
    lines     <- fmap (map splitAndTrim) $ splitIntoLines str
    firstName <- getFirstName $ head lines
    lastName  <- getLastName  $ lines !! 1
    age       <- getAge       $ lines !! 2
    Right $ Person firstName lastName age