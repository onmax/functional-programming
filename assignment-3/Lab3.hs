-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab3(Fruit(Apple,Banana,Lemon),sumPrice,BSTree(Void,BSNode),subTree,Tree(Node),count,labels,height,(++),elem,last,reverse,filter) where

import Prelude hiding (elem,last,reverse,filter, (++)) --(++), 

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1.1 -}
-- remember to provide the datatype representation
data Fruit = Apple Double | Banana Double | Lemon Integer -- modify as needed

{- 1.2 -}
-- remember to provide a function specification
sumPrice :: [Fruit] -> Double -> Double -> Double -> Double
sumPrice [] _ _ _ = 0.0
sumPrice (Apple n:fruits) apple_price banana_price lemon_price = apple_price * n + sumPrice fruits apple_price banana_price lemon_price
sumPrice (Banana n:fruits) apple_price banana_price lemon_price = banana_price * n + sumPrice fruits apple_price banana_price lemon_price 
sumPrice (Lemon n:fruits) apple_price banana_price lemon_price = lemon_price * (fromIntegral n) + sumPrice fruits apple_price banana_price lemon_price

{- 2 -}

{- Binary search trees

   Void represents an empty tree. BSNode l x r represents a tree with
   left subtree l, root label x, and right subtree r.

   INVARIANT: in every tree of the form BSNode l x r, all labels in l
     are < x, and all labels in r are > x.
 -}
data BSTree = Void | BSNode BSTree Integer BSTree  -- do not modify this line

-- remember to provide a function specification
deeperValuesExist :: Integer -> Integer -> BSTree -> Bool
deeperValuesExist min max Void = False
deeperValuesExist min max (BSNode l v r) = if v < max && v >= min then True else deeperValuesExist min max l || deeperValuesExist min max r
subTree min max Void = Void
subTree min max (BSNode l v r) | v < max && v >= min = BSNode (subTree min max l) v (subTree min max r)
                                | otherwise = if deeperValuesExist min max l then l else if deeperValuesExist min max r then r else Void

{- 3.1 -}
-- remember to provide the datatype representation
data Tree a = Node a [Tree a] -- modify as needed

{- 3.2 a) -}
-- remember to provide a function specification
--count [] = 0
--count [el1 : array] = el1 + count array
--count [a] = 1 + count a
counter [] = 0
counter (a:array) = count a + counter array
count (Node a array) = 1 + counter array

{- 3.2 b) -}
-- remember to provide a function specification
label [] = []
label (el:array) = labels el ++ label array
labels (Node a array) = [a] ++ label array

{- 3.2 c) -}
-- remember to provide a function specification
height (Node a []) = 1
height (Node a array) = maximum(map height array)

{- 4.1 -}
-- remember to provide a function specification
(++) array1 array2 = foldr (\acc next -> acc : next ) array2 array1


{- 4.2 -}
-- remember to provide a function specification
elem a array = foldl(\acc next -> if a == next then True else acc) False array

{- 4.3 -}
-- remember to provide a function specification
last (a:array) = foldl (\_ next -> next) a array

{- 4.4 -}
-- remember to provide a function specification
reverse array = foldl (\acc next -> next : acc ) [] array

{- 4.5 -}
-- remember to provide a function specification
--filter function array = foldl (\filtered next -> if function next then next : filtered else filtered) [] array
filter f array = foldl (\acc next -> if f next then acc ++ [next] else acc ) [] array
