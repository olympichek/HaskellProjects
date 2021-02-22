import Data.List ( unfoldr )
import Control.Arrow ( (>>>) )

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

toInt :: Z -> Int
toInt (Z _ []) = 0
toInt (Z s x)  = toInt' x where
    bitToNum One  = 1
    bitToNum Zero = 0
    negateIf Minus x = negate x
    negateIf Plus x  = x
    toInt' = map bitToNum
        >>> zipWith (\n b -> b * 2 ^ n) [0..]
        >>> sum
        >>> negateIf s

fromInt :: Int -> Z
fromInt x = Z sign res where
    (sign, res) = if x >= 0
        then (Plus, fromInt' x)
        else (Minus, fromInt' (-x))
    numToBit 1 = One
    numToBit 0 = Zero
    g x = if x > 0
        then Just (x `rem` 2, x `div` 2)
        else Nothing
    fromInt' = unfoldr g >>> map numToBit
    
add :: Z -> Z -> Z
add a b = fromInt $ toInt a + toInt b

mul :: Z -> Z -> Z
mul a b = fromInt $ toInt a * toInt b