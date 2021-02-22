data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a)   = 0
height (Node l r) = height l `max` height r + 1

size :: Tree a -> Int
size (Leaf a)   = 1
size (Node l r) = size l + size r + 1

avg :: Tree Int -> Int
avg t = s `div` c where
    (c, s) = go t
    go :: Tree Int -> (Int, Int)
    go (Leaf a) = (1, a)
    go (Node l r) = (lc + rc, ls + rs)
        where
        (lc, ls) = go l
        (rc, rs) = go r