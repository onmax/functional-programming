-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab3(Fruit(Apple,Banana,Lemon),sumPrice,BSTree(Void,BSNode),subTree,Tree(Node),count,labels,height,(++),elem,last,reverse,filter) where

import Prelude hiding (elem,last,reverse,filter, (++)) --(++), 

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1.1 -}
{- Structure that represent:
     1. Weight of apples
     2. Weight of bananas
     3. Number of lemons
-}
data Fruit = Apple Double | Banana Double | Lemon Integer

{- 1.2 -}
{- sumPrice [Fruit] a b c
     Calculates the price of all fruits in the parameter array with the prices given by the other paramters.
     RETURNS: The total price as a number
     EXAMPLES: sumPrice [Banana 4.0, Apple 3.0, Lemon 7, Banana 2.0, Apple 1.0, Lemon 1] 3.0 2.0 5.0 = 64.0
     sumPrice [] 3.0 2.0 5.0 = 0.0
-}
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

{- subTree min max Tree
     Checks if the current node is between min and max to trim the tree. It uses deeperValuesExist to check children's children.
     RETURNS: Returns a pruned tree
     EXAMPLES: subTree 2 3 (BSNode Void 0 (BSNode Void 2 Void)   ) 3 (BSNode Void 5 Void) = (BSNode Void 2 Void) 3 Void
-}
subTree :: Integer -> Integer -> BSTree -> BSTree
subTree min max Void = Void
subTree min max (BSNode l v r) 
          | v < max && v >= min = BSNode (subTree min max l) v (subTree min max r)
          | v >= max = subTree min max l
          | v < min = subTree min max r


{- 3.1 -}
{- Structure that represent a tree. First element is the value of the node, the second element is a list of the children
-}
data Tree a = Node a [Tree a]

{- 3.2 a) -}
{- counter [Tree]
       Counts the number of elements given the list of children of a node
       RETURNS: Number of elements in total in the list
       EXAMPLES: counter [Node X []] = 1
                 counter [Node X [Node Y [Node Z]]] = 3  
-}
counter [] = 0
counter (a:array) = count a + counter array

{- counter Tree
       Counts the number of elements in a tree
       RETURNS: Number of elements in total in the tree
       EXAMPLES: count Node X1 [Node X2 []] = 2
                 count Node X1 [Node X2 [Node X3 [Node X4]]] = 4
-}
count (Node a array) = 1 + counter array

{- 3.2 b) -}
{- label [Tree]
       Obtains the values of the nodes given the list of children of a node
       RETURNS: List of values of the nodes
       EXAMPLES: counter [Node X []] = [X]
                 counter [Node X [Node Y [Node Z]]] = [X, Y, Z]
-}
label [] = []
label (el:array) = labels el ++ label array
{- labels Tree
       Obtains the values of the nodes given a tree
       RETURNS: List of values of the nodes
       EXAMPLES: count Node X1 [Node X2 []] = [X1, X2]
                 count Node X1 [Node X2 [Node X3 [Node X4]]] = [X1, X2, X3, X4]
-}
labels (Node a array) = [a] ++ label array

{- 3.2 c) -}
{- height Tree
     Calculates the height of a tree.
     RETURNS: Number
     EXAMPLES: height (Node 1 [Node 2 []]) = 2
-}
height (Node a []) = 1
height (Node a array) = 1 + maximum(map height array)

{- 4.1 -}
{- (++) array1 array2
     Adds two arrays.
     RETURNS: Array
     EXAMPLES: (++) [1] [1,2,3] = [1,1,2,3]
               (++) [] [1,2,3] = [1,2,3]
-}
(++) array1 array2 = foldr (\acc next -> acc : next ) array2 array1


{- 4.2 -}
{- elem a array
     Checks if the array contains a.
     RETURNS: Boolean
     EXAMPLES: elem 1 [1,2,3] = True
               elem 'a' [1,2,3] = False
-}
elem a array = foldl(\acc next -> if a == next then True else acc) False array

{- 4.3 -}
{- last array
     Returns last element of array.
     PRE: Array cannot be empty
     RETURNS: Last element
     EXAMPLES: last [1,2,3] = 3
               last [] = Exception: Empty array
-}
last [] = error "Empty array"
last (a:array) = foldl (\_ next -> next) a array

{- 4.4 -}
{- reverse array
     Reverses array elements.
     RETURNS: Reversed array.
     EXAMPLES: reverse [1,2,3] = [3,2,1]
-}
reverse array = foldl (\acc next -> next : acc ) [] array

{- 4.5 -}
{- filter (a -> Bool) array
     Filters the given array with function.
     RETURNS: Returns the filtered array.
     EXAMPLES: filter (<5) [1,2,3,4,200,100,5] = [1,2,3,4]
-}
--filter function array = foldl (\filtered next -> if function next then next : filtered else filtered) [] array
filter f array = foldl (\acc next -> if f next then acc ++ [next] else acc ) [] array
