import Text.Read       ( readMaybe )
import Data.List.Split ( splitOn )
import Control.Monad   ( (<=<), (>=>) )
import Control.Arrow   ( (>>>) )
 
data Token = Number Double
    | Plus      | Minus
    | Mul       | Div
    | LeftBrace | RightBrace 
    deriving (Show, Eq)

spChars :: [Char]
spChars = "+-*/()"

spTokens :: [Token]
spTokens = [Plus, Minus, Mul, Div, LeftBrace, RightBrace]

changeBrackets :: Char -> Char 
changeBrackets '{' = '('
changeBrackets '}' = ')'
changeBrackets '[' = '('
changeBrackets ']' = ')'
changeBrackets c   = c

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy c = splitOn [c] >>> concatMap (: [[c]]) >>> init

splitAll :: String -> [String]
splitAll = foldr1 (<=<) $ map splitBy spChars

data Error = SyntaxError | ParseError | BracketsMismatch 
    deriving (Show, Eq)

asToken :: String -> Either Error Token
asToken s = case asSpToken of
    Just t  -> Right t
    Nothing -> case asNumber of
        Just n  -> Right . Number $ n
        Nothing -> Left SyntaxError
    where
    spStrings = map pure spChars
    asSpToken = lookup s $ zip spStrings spTokens
    asNumber = readMaybe s

removeMinus :: [Token] -> [Token]
removeMinus (Minus:(Number n):ts) = Number (-n) : ts
removeMinus ts = ts

removeMinuses :: [Token] -> [Token]
removeMinuses = foldr filterF [] where
    filterF t ts = case ts of
        (LeftBrace:Minus:(Number n):ts')
            -> t : LeftBrace : Number (-n) : ts'
        _   -> t : ts

tokenize :: String -> Either Error [Token]
tokenize = map changeBrackets
    >>> splitAll
    >>> filter (any (/= ' '))
    >>> mapM asToken
    >>> fmap removeMinus
    >>> fmap removeMinuses

checkBrackets :: [Token] -> Maybe Bool
checkBrackets ts
    | lb /= rb  = Nothing
    | lb == 0   = Just False
    | otherwise = Just True
    where
    lb = length $ filter (== LeftBrace) ts
    rb = length $ filter (== RightBrace) ts

infixl 6 :+:, :-:
infixl 7 :*:, :/:
data Expr = Val Double
    | Expr :+: Expr | Expr :-: Expr
    | Expr :*: Expr | Expr :/: Expr
    deriving (Show, Eq)

action :: Token -> Expr -> Expr -> Expr
action Plus  = (:+:)
action Minus = (:-:)
action Mul   = (:*:)
action Div   = (:/:)

build :: [Expr] -> [Expr -> Expr -> Expr] -> Either Error Expr
build [v]        []       = Right v
build (v1:v2:vs) (op:ops) = build (v1 `op` v2 : vs) ops
build _          _        = Left ParseError

solveMulDiv :: [Token] -> Either Error Expr
solveMulDiv ts = build (Val <$> ns) (action <$> ops)
    where
    (ns, ops) = foldr split ([], []) ts
    split t (ns, ops) = case t of
        Number n -> (n:ns, ops)
        _        -> (ns, t:ops)
    
solvePlusMinus :: [Token] -> Either Error Expr
solvePlusMinus ts = do
    exprs <- mapM solveMulDiv tss
    build exprs (action <$> ops)
    where
    tss = concatMap (splitOn [Minus]) $ splitOn [Plus] ts
    ops = filter (\x -> x == Plus || x == Minus) ts

eval :: Expr -> Double
eval (Val n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :-: e2) = eval e1 - eval e2
eval (e1 :*: e2) = eval e1 * eval e2
eval (e1 :/: e2) = eval e1 / eval e2

splitByBrackets :: [Token] -> ([Token], [Token], [Token])
splitByBrackets ts = (lt, ct, rt) where
    t1 = splitOn [LeftBrace] ts
    t2 = concat $ tail t1
    t3 = splitOn [RightBrace] t2
    lt = head t1
    ct = concat $ init t3
    rt = last t3

compute :: [Token] -> Either Error Double
compute ts
    | checkBrackets ts == Just True  = nt >>= compute
    | checkBrackets ts == Just False = eval <$> solvePlusMinus ts
    | otherwise                      = Left BracketsMismatch
    where
    (lt, ct, rt) = splitByBrackets ts
    nt = do
        res <- compute ct
        return $ lt ++ [Number res] ++ rt

main :: IO ()
main = do
    putStr "Enter expression: "
    expr <- getLine
    case (tokenize >=> compute) expr of
        Right n -> putStrLn $ "Result: " ++ show n
        Left e  -> putStrLn $ "Error: "  ++ show e