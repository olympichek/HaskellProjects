import Control.Monad ( forM_, when )
import Control.Monad.ST ( runST )
import Data.STRef ( modifySTRef, newSTRef, readSTRef )
import Data.Vector ( Vector )
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

myVector :: Vector Int
myVector = Vector.fromList [52, 23, 17, 47]

mySum :: Num a => Vector a -> a
mySum v
    | null v = 0
    | otherwise = Vector.head v + mySum (Vector.tail v)

mySumST :: Num a => Vector a -> a
mySumST v = runST $ do
    s <- newSTRef 0
    forM_ v $ \x -> modifySTRef s (+x)
    readSTRef s

myBubbleSort :: Ord a => Vector a -> Vector a
myBubbleSort v = runST $ do
    mv <- Vector.thaw v
    let n = MVector.length mv
    forM_ [0..(n-2)] $ \i ->
        forM_ [0..(n-i-2)] $ \j -> do
            a <- MVector.read mv j
            b <- MVector.read mv (j+1)
            when (a > b) $ MVector.swap mv j (j+1)
    Vector.freeze mv

main :: IO ()
main = do
    let s1 = mySum myVector
        s2 = mySumST myVector
        sv = myBubbleSort myVector
    putStrLn $ "sum: " ++ show s1
    putStrLn $ "sumST: " ++ show s2
    putStrLn $ "sorted vector: " ++ show sv
