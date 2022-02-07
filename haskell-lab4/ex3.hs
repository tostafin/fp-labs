data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt


data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt


data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"


depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + depthOfBT lt + depthOfBT rt


-- flattenBT :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder

preorderBT :: BinTree a -> [a]
preorderBT EmptyBT = []
preorder (NodeBT root left right) = [root] ++ (preorderBT left) ++ (preorderBT right)

inorderBT :: BinTree a -> [a]
inorderBT EmptyBT = []
inorderBT (NodeBT root left right) = (inorderBT left) ++ [root] ++ (inorderBT right)

postorderBT :: BinTree a -> [a]
postorderBT EmptyBT = []
postorderBT (NodeBT root left right) = (postorderBT left) ++ (postorderBT right) ++ [root]


mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT root left right) = NodeBT (f root) (mapBT f left) (mapBT f right)


insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT root left right) = (NodeBT root (insert x left) right)


insertBST :: Ord a => BinTree a -> a -> BinTree a
insertBST EmptyBT x = NodeBT x EmptyBT EmptyBT
insertBST (NodeBT root left right) x | x > root = (NodeBT root left (insertBST right x))
                                     | x < root = (NodeBT root (insertBST left x) right)


list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST [] = EmptyBT
list2BST (x:xs) = foldl insertBST EmptyBT (x:xs)


