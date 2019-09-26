-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab3(Fruit(Apple,Banana,Lemon),sumPrice,BSTree(Void,BSNode),subTree,Tree(Node),count,labels,height,(++),elem,last,reverse,filter) where

import Prelude hiding ((++),elem,last,reverse,filter)

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1.1 -}
-- remember to provide the datatype representation
data Fruit = Apple | Banana | Lemon  -- modify as needed

{- 1.2 -}
-- remember to provide a function specification
sumPrice = undefined  -- remove "undefined" and write your function here

{- 2 -}

{- Binary search trees

   Void represents an empty tree. BSNode l x r represents a tree with
   left subtree l, root label x, and right subtree r.

   INVARIANT: in every tree of the form BSNode l x r, all labels in l
     are < x, and all labels in r are > x.
 -}
data BSTree = Void | BSNode BSTree Integer BSTree  -- do not modify this line

-- remember to provide a function specification
subTree = undefined  -- remove "undefined" and write your function here

{- 3.1 -}
-- remember to provide the datatype representation
data Tree a = Node  -- modify as needed

{- 3.2 a) -}
-- remember to provide a function specification
count = undefined  -- remove "undefined" and write your function here

{- 3.2 b) -}
-- remember to provide a function specification
labels = undefined  -- remove "undefined" and write your function here

{- 3.2 c) -}
-- remember to provide a function specification
height = undefined  -- remove "undefined" and write your function here

{- 4.1 -}
-- remember to provide a function specification
(++) = undefined  -- remove "undefined" and write your function here

{- 4.2 -}
-- remember to provide a function specification
elem = undefined  -- remove "undefined" and write your function here

{- 4.3 -}
-- remember to provide a function specification
last = undefined  -- remove "undefined" and write your function here

{- 4.4 -}
-- remember to provide a function specification
reverse = undefined  -- remove "undefined" and write your function here

{- 4.5 -}
-- remember to provide a function specification
filter = undefined  -- remove "undefined" and write your function here
