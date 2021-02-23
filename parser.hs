import System.IO       ( hFlush, stdout )
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

splitBy :: String -> String -> [String]
splitBy s = splitOn s >>> concatMap (: [s]) >>> init

splitAll :: String -> [String]
splitAll = foldr1 (<=<) $ splitBy . pure <$> spChars

data Error = LexicalError | ParseError | BracketsMismatch 
    deriving (Show, Eq)

asToken :: String -> Either Error Token
asToken s = case asSpToken of
    Just t  -> Right t
    Nothing -> case asNumber of
        Just n  -> Right . Number $ n
        Nothing -> Left LexicalError
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

buildExpr :: [Expr] -> [Expr -> Expr -> Expr] -> Either Error Expr
buildExpr [v]        []       = Right v
buildExpr (v1:v2:vs) (op:ops) = buildExpr (v1 `op` v2 : vs) ops
buildExpr _          _        = Left ParseError

buildMulDiv :: [Token] -> Either Error Expr
buildMulDiv ts = buildExpr (Val <$> ns) (action <$> ops)
    where
    (ns, ops) = foldr split ([], []) ts
    split t (ns, ops) = case t of
        Number n -> (n:ns, ops)
        _        -> (ns, t:ops)
    
buildPlusMinus :: [Token] -> Either Error Expr
buildPlusMinus ts = do
    exprs <- mapM buildMulDiv tss
    buildExpr exprs (action <$> ops)
    where
    tss = concatMap (splitOn [Minus]) $ splitOn [Plus] ts
    ops = filter (\x -> x == Plus || x == Minus) ts

eval :: Expr -> Double
eval (Val n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :-: e2) = eval e1 - eval e2
eval (e1 :*: e2) = eval e1 * eval e2
eval (e1 :/: e2) = eval e1 / eval e2

splitByBrackets' :: [Token] -> ([Token], [Token], [Token])
splitByBrackets' ts = (lt, ct, rt) where
    t1 = splitOn [LeftBrace] ts
    t2 = concat $ tail t1
    t3 = splitOn [RightBrace] t2
    lt = head t1
    ct = concat $ init t3
    rt = last t3

splitByBrackets :: [Token] -> [[Token]]
splitByBrackets [] = [[]]
splitByBrackets ts = helper ts 0 [[]]  where
    helper []     _ res = res
    helper (t:ts) n res = case t of
        LeftBrace  -> if n == 0
            then helper ts (n+1) $ res ++ [[]]
            else helper ts (n+1) $ res +++ [t]
        RightBrace -> if n == 1
            then helper ts (n-1) $ res ++ [[]]
            else helper ts (n-1) $ res +++ [t]
        _          -> helper ts n $ res +++ [t]
    xs +++ ys = init xs ++ [last xs ++ ys]

compute :: [Token] -> Either Error Double
compute ts = case checkBrackets ts of
    Just True  -> nts >>= compute
    Just False -> eval <$> buildPlusMinus ts
    Nothing    -> Left BracketsMismatch
    where
    nts = helper $ splitByBrackets ts
    helper [p1] = Right p1
    helper (p1:p2:ps) = do
        r <- compute p2
        rs <- helper ps
        return $ p1 ++ [Number r] ++ rs

main :: IO ()
main = do
    putStr "Enter expression: " >> hFlush stdout
    expr <- getLine
    case tokenize >=> compute $ expr of
        Right n -> putStrLn $ "Result: " ++ show n
        Left e  -> putStrLn $ "Error: "  ++ show e