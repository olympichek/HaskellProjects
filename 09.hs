import Control.Monad.State.Lazy

type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x:xs) = (x, xs)
      
-- push :: Int -> Stack -> ((), Stack)
-- push a xs = ((), a:xs)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)
      
push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack ()  
stackManip = do
    a <- pop
    push 3
    push a

main :: IO ()
main = 
    print $ runState stackManip [5,8,2,1]  