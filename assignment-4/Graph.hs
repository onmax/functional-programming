module Graph where  -- modify as needed

{-
    Graph is represented with two lists:
    1. The first one is a list with all the vertices of the graph.
    2. The second list is a list of tuples representing all edges. All vertices in this list must be added
        before in the vertices list.
    This graph is an undirected (relation a->b is the same as b->a) and unweighted graph.
-}
data Graph a = Graph [a] [(a, a)] deriving (Show)

{- 1.1 -}
{-
    empty
        Creates a empty graph: empty list of vertices and empty list of edges
        RETURNS: Empty graph
        EXAMPLE empty = Graph [] []
-}
empty :: Graph a
empty = Graph [] []

{- 1.2 -}
{-
    empty
        Add a vertex in the graph
        RETURNS: The new graph with the new vertex
        PRE: Vertex must not be in the vertices list
        EXAMPLES addVertex Graph [1,2] [] 3 = Graph [1,2,3] []
                 addVertex Graph ["a", "b"] [("a","b")] "c" = Graph ["a", "b", "c"] [("a","b")]
                 addVertex Graph [1,2] [] 1 = Graph [1,2] []
-}
addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (Graph v e) new_vertex | notElem new_vertex v = Graph (v ++ [new_vertex]) e
addVertex (Graph v e) _ = Graph v e

{- 1.3 -}
{-
    addEdge
        Add a edge in the graph
        RETURNS: The new graph with the new edge
        PRE: Edge must not be in the edges list and both vertices must be in the vertices list
        EXAMPLES addEdge Graph [1,2] [] (1,2) = Graph [1,2] [(1,2)]
                 addEdge Graph [1,2,3,4] [(3,4),(3,2)] (1,2) = Graph [1,2,3,4] [(3,4),( 3,2), (1,2)]
                 addEdge Graph ["a", "b"] [("a","b")] ("a","b") = Graph ["a", "b"] [("a","b")]
                 addEdge Graph [1,2] [] (3,4) = Graph [1,2] []
-}
addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge (Graph v e) (a,b) | elem a v && elem b v && notElem (a,b) e && notElem (b,a) e  = Graph v (e ++ [(a,b)])
addEdge (Graph v e) _ = Graph v e

{- 1.4 -}
{-
    vertices
        RETURNS: Return the list of the vertices
        EXAMPLES vertices Graph [] []= []
                 vertices Graph [1,2,3,4] [(3,4),(3,2)] = [1,2,3,4]
                 vertices Graph ["a", "b"] [("a","b")] = ["a", "b"]
                 vertices Graph [1,2] [] = [1,2]
-}
vertices :: Eq a => Graph a -> [a]
vertices (Graph v _) = v

{- 1.5 -}
{-
    vertices
        Creates a list of the first-level neighbors given a vertex
        RETURNS: Return a list of the neighbors of a vertex
        PRE: Vertex must be in the list of vertices
        EXAMPLES neighbors Graph [] [] "a" = []
                 neighbors Graph [1,2,3,4] [(3,4),(3,2)] 3 = [4,2]
                 neighbors Graph ["a", "b"] [] "a" = []
-}
neighbors :: Eq a => Graph a -> a -> [a]
neighbors (Graph _ []) _ = []
neighbors (Graph v ((v1,v2):e)) vertex | v1 == vertex = v2 : neighbors (Graph v e) vertex
neighbors (Graph v ((v1,v2):e)) vertex | v2 == vertex = v1 : neighbors (Graph v e) vertex
neighbors (Graph v (_:e)) vertex = neighbors (Graph v e) vertex

{- 2 -}
-- For all operations, state their (worst-case) time complexity,
-- expressed as a function of the number of vertices or edges (or
-- both) that are contained in the graph.
{-
Complexity empty
T() = O(1)

Complexity addVertex
T(nv) = O(nv); nv = # of vertexes

Complexity addEdges
T(nv, ne) = O(2 * nv + 2 * ne); nv = # of vertexes, ne = # of edges

Complexity vertices
T() = O(1)

Complexity neighbors
T(ne) = O(ne); ne = # of edges
-}
