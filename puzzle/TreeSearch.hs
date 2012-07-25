module TreeSearch where


class TreeNode a where
    generateChildren :: a -> [a]
    stopLooking      :: a -> Bool
    isSolution       :: a -> Bool

search :: TreeNode a => a -> [a]
search node
    | isSolution node  = [node]
    | stopLooking node = []
    | otherwise        = concatMap search (generateChildren node)

