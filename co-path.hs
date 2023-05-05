module Main where

import Data.Graph.UGraph
import Data.Graph.Types
import Data.Graph.Traversal
import Data.List
import System.Environment (getArgs)


parse_edges :: [String] -> [Edge Int ()]
parse_edges [] = []
parse_edges (curr_line : rest) =
  let [from, to] = words curr_line
      from_vertex = read from :: Int
      to_vertex = read to :: Int
  in Edge from_vertex to_vertex () : (parse_edges rest)


parse_graph :: String -> UGraph Int ()
parse_graph contents =
  let (vertices : edges_lines) = lines contents
      num_vertices = read vertices :: Int
      edges = parse_edges edges_lines
  in insertVertices [1..num_vertices] (fromEdgesList edges)
-- ensures that all vertices are inserted even if they dont have any edges


connected_components :: UGraph Int () -> [[Int]]
connected_components graph =
  let list_vertices = vertices graph
  in find_components graph list_vertices

find_components :: UGraph Int () -> [Int] -> [[Int]]
find_components graph [] = []
find_components graph remaining_vertices =
  let component = dfsVertices graph (head remaining_vertices)
      leftovers = filter (\item -> item `notElem` component) remaining_vertices
  in (component : (find_components graph leftovers))

find_cycle_edge :: [[Int]] -> UGraph Int () -> Maybe (Edge Int ())
find_cycle_edge [] _ = Nothing
find_cycle_edge (curr_component : rest) graph =
    let is_degree2 = foldl (\satisfies vertex -> ((vertexDegree graph vertex) == 2) && satisfies) True curr_component
        all_edges_in_component = foldl (\edges vertex -> Data.List.union edges (incidentEdges graph vertex))
                                      [] curr_component
    -- the component must be a cycle graph if every vertex has degree 2 and number of vertices = number of edges
    -- Resource: https://en.wikipedia.org/wiki/Cycle_graph
    in if is_degree2 && (length all_edges_in_component == length curr_component)
        then Just (head all_edges_in_component) -- return and this arbitrary edge in a cycle will get removed
        else find_cycle_edge rest graph

checkR1 :: UGraph Int () -> Maybe (UGraph Int (), [Edge Int ()])
checkR1 graph =
  let components = connected_components graph
  in case (find_cycle_edge components graph) of
      Just edge -> Just ((removeEdge edge graph), [edge])
      Nothing -> Nothing


find_deg_ge3_vertices :: UGraph Int () -> [Int]
find_deg_ge3_vertices graph =
  let degree_of_vertices = zip (vertices graph) (degrees graph)
      vertices_deg_ge3 = filter (\(_, degree) -> degree >= 3) degree_of_vertices
  in map (\(vertex, degree) -> vertex) vertices_deg_ge3

-- output all combinations of size 2 in the input list
choose2 :: [a] -> [(a, a)]
choose2 [] = []
choose2 (h:t) =
  let head_combinations = map (\item -> (h, item)) t
  in head_combinations ++ choose2 t

is_valid_u1_u2 :: (Edge Int (), Edge Int ()) -> [Edge Int ()] -> UGraph Int () -> Int -> Bool
is_valid_u1_u2 (u1, u2) all_incident_edges graph vertex =
  let deleting_edges = all_incident_edges \\ [u1, u2]
      g' = removeEdges deleting_edges graph
      [new_component] = find_components g' [vertex]
  in is_path_component new_component g'

find_u1_u2 :: Int -> UGraph Int () -> Maybe (Edge Int (), Edge Int ())
find_u1_u2 vertex graph =
  let incident = incidentEdges graph vertex
      possible_combinations = choose2 incident
      validate_u1_u2 = map (\item -> is_valid_u1_u2 item incident graph vertex) possible_combinations
      combination_is_valid = zip possible_combinations validate_u1_u2
      valid_u1_u2 = (filter (\(_, is_valid) -> is_valid) combination_is_valid)
  in case (head valid_u1_u2) of
    ((u1, u2), True) -> Just (u1, u2)
    _ -> Nothing

check_vertices_satisfy_R2 :: [Int] -> UGraph Int () -> Maybe (UGraph Int (), [Edge Int ()])
check_vertices_satisfy_R2 [] _ = Nothing
check_vertices_satisfy_R2 (h:t) graph =
  case (find_u1_u2 h graph) of
    Just (u1, u2) ->
      let v_edges = incidentEdges graph h
          edges_deleted = v_edges \\ [u1, u2]
          new_graph = removeEdges edges_deleted graph
      in Just (new_graph, edges_deleted)
    Nothing -> check_vertices_satisfy_R2 t graph

checkR2 :: UGraph Int () -> Maybe (UGraph Int (), [Edge Int ()])
checkR2 graph =
  let all_possible_v = find_deg_ge3_vertices graph
  in check_vertices_satisfy_R2 all_possible_v graph


checkB1 :: UGraph Int () -> Maybe (UGraph Int (), [Edge Int ()])
checkB1 graph = Nothing

checkB2 :: UGraph Int () -> Maybe (UGraph Int (), [Edge Int ()])
checkB2 graph = Nothing

update_count_tuple :: Maybe (Int, Int) -> Int -> Maybe (Int, Int)
update_count_tuple Nothing _ = Nothing
update_count_tuple (Just (deg1, deg2)) curr_degree =
  case curr_degree of
    1 -> Just (deg1 + 1, deg2)
    2 -> Just (deg1, deg2 + 1)
    _ -> Nothing

degree_count :: [Int] -> UGraph Int () -> Maybe (Int, Int)
degree_count curr_component graph= foldl (\degree_count vertex -> update_count_tuple degree_count (vertexDegree graph vertex))
                                         (Just (0, 0)) curr_component


is_path_component :: [Int] -> UGraph Int () -> Bool
is_path_component [] _ = True
-- The component is a path if all vertices except 2 have degree 2 and the 2 terminal vertices have degree 1
-- Source: https://en.wikipedia.org/wiki/Path_graph
is_path_component component graph =
  let degrees = foldl (\degree_count vertex -> update_count_tuple degree_count (vertexDegree graph vertex))
                        (Just (0, 0)) component
  in case degrees of
      Nothing -> False
      Just (deg1, deg2) -> deg1 == 2 && deg2 == (length component) - 2

is_co_path_helper :: [[Int]] -> UGraph Int () -> Bool
is_co_path_helper [] _ = True
is_co_path_helper (curr : rest) graph =
  (is_path_component curr graph) && (is_co_path_helper rest graph)

is_co_path :: UGraph Int () -> Bool
is_co_path graph =
  let components = connected_components graph
  in is_co_path_helper components graph

find_co_path_set :: UGraph Int () -> [Edge Int ()] -> Int -> Maybe [Edge Int ()]
find_co_path_set graph delete_set k =
  if is_co_path graph
    then Just delete_set
    else
      case (checkR1 graph, checkR2 graph, checkB1 graph, checkB2 graph, k <= 0) of
        (_,_,_,_, True) -> Nothing
        (Just (new_graph, delete_set), _,_,_,_) -> find_co_path_set new_graph delete_set (k-1)
        (_,Just (new_graph, delete_set), _,_,_) -> find_co_path_set new_graph delete_set (k - (length delete_set))
        (_,_,Just (new_graph, delete_set),_,_) -> find_co_path_set new_graph delete_set (k - (length delete_set))
        (_,_,_,Just (new_graph, delete_set),_) -> find_co_path_set new_graph delete_set (k - (length delete_set))



main :: IO ()
main =
  getArgs >>= go
  where
    go :: [String] -> IO ()
    go [filename, k_str] = do
      file <- readFile filename
      let graph = parse_graph file
          k = read k_str :: Int
      case (find_co_path_set graph [] k) of
        Just delete_set -> putStrLn (show delete_set)
        Nothing -> putStrLn "Not possible"

    go _ = putStrLn "Error: Syntax. Usage: ./co-path [filename] [k]"
