import Prelude hiding ( lookup )
import qualified Data.List as L

class MapLike m where
    empty    :: m k v
    lookup   :: Ord k => k -> m k v -> Maybe v
    insert   :: Ord k => k -> v -> m k v -> m k v
    delete   :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k, v)] }
    deriving (Eq, Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap m) = L.lookup k m
    insert k v (ListMap m) = case L.lookup k m of
        Just _  -> ListMap $ foldr f [] m where
            f (k', v') ps = if k == k' then (k', v):ps else (k', v'):ps
        Nothing -> ListMap $ (k, v):m
    delete k (ListMap m) = ListMap $ filter ((/= k) . fst) m

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap $ const Nothing
    lookup k (ArrowMap f) = f k
    insert k v (ArrowMap f) = ArrowMap f' where
        f' k' = if k' == k then Just v else f k'
    delete k (ArrowMap f) = ArrowMap f' where
        f' k' = if k' == k then Nothing else f k'
    fromList = ArrowMap . flip L.lookup
