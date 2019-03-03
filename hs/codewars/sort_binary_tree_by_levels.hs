-- https://www.codewars.com/kata/sort-binary-tree-by-levels

module TreeByLevels where

import TreeByLevels.TreeNode

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels x = do
  nodes <- levels x
  map value nodes

levels :: Maybe (TreeNode a) -> [[(TreeNode a)]]
levels Nothing = []
levels (Just node) = ([node]:others) where
  others = zipWithDefault [] [] (++) (levels $ left node) (levels $ right node)

zipWithDefault :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault dx _  f []     ys     = zipWith f (repeat dx) ys
zipWithDefault _  dy f xs     []     = zipWith f xs (repeat dy)
zipWithDefault dx dy f (x:xs) (y:ys) = f x y : zipWithDefault dx dy f xs ys
