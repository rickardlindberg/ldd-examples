module TreeSearch where


search :: (a -> Bool) -> (a -> Bool) -> (a -> [a]) -> a -> [a]
search isSolution stopLooking generateChildren node
    | isSolution node  = [node]
    | stopLooking node = []
    | otherwise        = concatMap (search isSolution stopLooking generateChildren) (generateChildren node)

