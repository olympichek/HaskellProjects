data Coord a = Coord a a
    deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter s (Coord x y) = Coord (f x') (f y') where
    x' = fromIntegral x
    y' = fromIntegral y
    f k = s * k + signum k * s / 2

getCell :: Double -> Coord Double -> Coord Int
getCell s (Coord x y) = Coord (f x) (f y) where
    f k = if k >= 0
        then floor $ k / s
        else ceiling $ k / s