module TreeSearch where


class TreeNode a where
    generateSubTree :: a -> [a]
    isBad           :: a -> Bool

search :: TreeNode a => a -> [a]
search node = []

