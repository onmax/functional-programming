module Graph where  -- modify as needed

-- remember to provide the datatype representation
data Graph a = Graph  -- modify as needed

{- 1.1 -}
empty :: Graph a
empty = undefined  -- remove "undefined" and write your definition here

{- 1.2 -}
-- remember to provide a function specification
addVertex :: Eq a => Graph a -> a -> Graph a
addVertex = undefined  -- remove "undefined" and write your function here

{- 1.3 -}
-- remember to provide a function specification
addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge = undefined  -- remove "undefined" and write your function here

{- 1.4 -}
-- remember to provide a function specification
vertices :: Eq a => Graph a -> [a]
vertices = undefined  -- remove "undefined" and write your function here

{- 1.5 -}
-- remember to provide a function specification
neighbors :: Eq a => Graph a -> a -> [a]
neighbors = undefined  -- remove "undefined" and write your function here

{- 2 -}
-- For all operations, state their (worst-case) time complexity,
-- expressed as a function of the number of vertices or edges (or
-- both) that are contained in the graph.
