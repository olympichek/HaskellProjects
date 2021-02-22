import Data.Char ( isDigit )
import Control.Monad ( guard )

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken s
    | all isDigit s = Just . Number . read $ s
    | otherwise     = Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words

lst :: [(Int, Int)]
lst = do
    x <- [1, 2, 3]
    y <- [1, 2]
    True <- return $ x /= y
    -- guard $ x /= y
    return (x, y)

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
    b <- [1..x]
    a <- [1..b-1]
    c <- [1..x]
    guard $ a^2 + b^2 == c^2
    return (a, b, c)