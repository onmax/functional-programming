-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab4(dot,connectedComponent) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

-- import your Graph module
import Graph
import Data.List

{- 3 -}
{-
    dot
        Creates a string representing the graph. The format of the string is:
                                                    graph {
                                                        v1;
                                                        v2;
                                                        v1-v2;
                                                    }
        RETURNS: A string representing the graph
        EXAMPLES dot Graph [] [] = graph {
                                    }
                 dot Graph [1,2,3,4] [(3,4),(3,2)] = graph {
                                                        1;
                                                        2;
                                                        3;
                                                        4;
                                                        3-4;
                                                        3-2;
                                                        }
                 dot Graph ["a", "b"] [] =  graph {
                                                a;
                                                b;
                                            }
-}
dot :: Graph String -> String
dot (Graph v e) = "graph {\n " ++ unwords (map(\x -> "  " ++ x ++ ";\n") v ++ map(\(e1,e2) -> "  " ++ e1 ++ " -- " ++ e2 ++ ";\n") e) ++ "}"

{- 4 -}              
{-
    dfs (Depth First Search)
        Creates a list of the vertices in the same component as the vertex given as an argument
        RETURNS: Return a list of the vertices in the same component as the vertex given in the input.
        PRE: Vertex must be in the list of vertices
        EXAMPLES dfs (Graph [] []) [] ["a"] = []
                 dfs (Graph [1,2,3,4] [(3,4),(3,2)]) [] [3] = [3,4,2]
                 dfs (Graph ["a","b","c","d","e"] [("a","b"),("a","c"),("b","c"),("d","e")]) [] ["a"] = ["a","b","c"]
                 dfs (Graph ["a","b","c","d","e"] [("a","b"),("a","c"),("b","c"),("d","e")]) [] ["d"] = ["d","e"]
                 dfs (Graph ["a", "b"] []) [] ["a"] = ["a"]
-}            
dfs :: Eq a => Graph a -> [a] -> [a] -> [a]
dfs _ visited [] = reverse visited
dfs (Graph v e) visited (x:xs)  
                            | notElem x v = reverse visited 
                            | elem x visited = dfs (Graph v e) visited xs
                            | otherwise = dfs (Graph v e) (x:visited) (new_vertexes ++ xs)
                                where
                                    new_vertexes = Graph.neighbors (Graph v e) x

{-
    connectedComponent
        Retrieves the component which the vertex is part of
        RETURNS: Return a a graph of the component.
        PRE: Vertex must be in the list of vertices
        EXAMPLES connectedComponent (Graph [] []) "a" = Graph [] []
                 connectedComponent (Graph [1,2,3,4] [(3,4),(3,2)]) 3 = Graph [3,4,2] [(3,4),(3,2)]
                 connectedComponent (Graph ["a","b","c","d","e"] [("a","b"),("a","c"),("b","c"),("d","e")]) "a" = Graph ["a","b","c"] [("a","b"),("a","c"),("b","c")]
                 connectedComponent (Graph ["a","b","c","d","e"] [("a","b"),("a","c"),("b","c"),("d","e")]) "d" = Graph ["d","e"] [("d","e")]
                 connectedComponent (Graph ["a", "b"] []) "a" = Graph ["a"] []
-}  
connectedComponent :: Eq a => Graph a -> a -> Graph a
connectedComponent (Graph v e) vertex = Graph vertexes edges
                                    where 
                                        vertexes = dfs (Graph v e) [] [vertex]
                                        edges = filter (\(e1, e2) -> elem e1 vertexes || elem e1 vertexes) e

                                    