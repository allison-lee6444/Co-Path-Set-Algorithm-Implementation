module Main where

import Data.Graph.UGraph
import Data.Graph.Types
import Data.Graph.Traversal
import Data.Graph.Generation
import Data.Graph.Visualize
import Data.Graph.Read

import Data.List
import System.Environment (getArgs)

import Debug.Trace

type Vertex = Int

-- parses each line of edges with format "[from_vertex] [to_vertex]"
parse_edges :: [String] -> [Edge Vertex ()]
parse_edges [] = []
parse_edges (curr_line : rest) =
  let [from, to] = words curr_line
      from_vertex = read from :: Vertex
      to_vertex = read to :: Vertex
  in Edge from_vertex to_vertex () : (parse_edges rest)


-- parses the entire file with format described in README
parse_graph :: String -> UGraph Vertex ()
parse_graph contents =
  let (vertices : edges_lines) = lines contents
      num_vertices = read vertices :: Int
      edges = parse_edges edges_lines
  in insertVertices [1..num_vertices] (fromEdgesList edges)
-- last line ensures that all vertices are inserted even if they dont have any edges


-- given a graph, output a list of connected components
connected_components :: UGraph Vertex () -> [[Vertex]]
connected_components graph =
  let list_vertices = vertices graph
  in find_components graph list_vertices

-- find the components that the given list of vertices belongs to. Will not output duplicate elements
find_components :: UGraph Vertex () -> [Vertex] -> [[Vertex]]
find_components graph [] = []
find_components graph remaining_vertices =
  let component = dfsVertices graph (head remaining_vertices)
      leftovers = filter (\item -> item `notElem` component) remaining_vertices
  in (component : (find_components graph leftovers))

-- find if there is a cycle component. If it does, output an arbitrary edge of the cycle
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

-- checks if the graph satisfies R1
checkR1 :: UGraph Int () -> Maybe (UGraph Int (), [Edge Int ()])
checkR1 graph =
  let components = connected_components graph
  in case (find_cycle_edge components graph) of
      Just edge -> Just ((removeEdge edge graph), [edge])
      Nothing -> Nothing

-- find vertices in the graph that has degree >=3
find_deg_ge3_vertices :: UGraph Int () -> [Int]
find_deg_ge3_vertices graph =
  let degree_of_vertices = zip (vertices graph) (degrees graph)
      vertices_deg_ge3 = filter (\(_, degree) -> degree >= 3) degree_of_vertices
  in map (\(vertex, degree) -> vertex) vertices_deg_ge3

-- output all combination of size k in the given list
choose_k :: Int -> [a] -> [[a]]
choose_k _ [] = []
choose_k 1 lst = map (\item -> [item]) lst
choose_k k (h:t) =
  let choice_has_h = map (\item -> h:item) (choose_k (k - 1) t)
      choice_has_no_h = choose_k k t
  in choice_has_h ++ choice_has_no_h

-- check if the two edges given are valid u1 and u2 for condition R2
is_valid_u1_u2 :: (Edge Int (), Edge Int ()) -> [Edge Int ()] -> UGraph Int () -> Int -> Bool
is_valid_u1_u2 (u1, u2) all_incident_edges graph vertex =
  let deleting_edges = all_incident_edges \\ [u1, u2]
      g' = removeEdges deleting_edges graph
      [new_component] = find_components g' [vertex]
  in is_path_component new_component g'

-- finds a valid u1 and u2 of a given edge for condition R2
find_u1_u2 :: Int -> UGraph Int () -> Maybe (Edge Int (), Edge Int ())
find_u1_u2 vertex graph =
  let incident = incidentEdges graph vertex
      possible_combinations = choose_k 2 incident
      validate_u1_u2 = map (\[u1, u2] -> is_valid_u1_u2 (u1, u2) incident graph vertex) possible_combinations
      combination_is_valid = zip possible_combinations validate_u1_u2
      valid_u1_u2 = (filter (\(_, is_valid) -> is_valid) combination_is_valid)
  in case (valid_u1_u2) of
    [] -> Nothing
    (([u1, u2], True) : t) -> Just (u1, u2)

-- check if any vertices satisfy the condition of R2
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

-- checks if the graph matches the conditions of R2
checkR2 :: UGraph Int () -> Maybe (UGraph Int (), [Edge Int ()])
checkR2 graph =
  let all_possible_v = find_deg_ge3_vertices graph
  in check_vertices_satisfy_R2 all_possible_v graph

-- find all degree 2 neighbors of a given vertex
find_degree2_neighbors :: [Int] -> UGraph Int () -> Maybe Int
find_degree2_neighbors [] _ = Nothing
find_degree2_neighbors (h:t) graph =
  case (vertexDegree graph h) of
    2 -> Just h
    _ -> find_degree2_neighbors t graph

-- as described in B1 or B2, output all possible E'
find_subset :: Int -> Maybe Int -> Int -> UGraph Int () -> [[Edge Int ()]]
find_subset v keep_vertex size graph =
  let incident = incidentEdges graph v
  in case keep_vertex of
      Nothing -> choose_k size incident
      Just u ->
        let e' = delete (Edge v u ()) incident
        in choose_k size e'

-- checks if the graph matches the condition of B1
checkB1 :: UGraph Int () -> Maybe (Int, [[Edge Int ()]])
checkB1 graph =
  let all_possible_v = find_deg_ge3_vertices graph
      v_neighbors = map (\v -> adjacentVertices graph v) all_possible_v
      degree2_neighbors = map (\neighbors -> find_degree2_neighbors neighbors graph) v_neighbors
      valid_v = filter (\(_, deg2_neighbor) -> deg2_neighbor /= Nothing) (zip all_possible_v degree2_neighbors)
  in case valid_v of
    [] -> Nothing
    _ ->
      let (chosen_v, Just chosen_u) = head valid_v
          degree_v = vertexDegree graph chosen_v
          deletion_branches = find_subset chosen_v (Just chosen_u) (degree_v - 2) graph
      in Just (degree_v, deletion_branches)

-- checks if the graph matches the condition of B2
checkB2 :: UGraph Int () -> Maybe (Int, [[Edge Int ()]])
checkB2 graph =
  let all_possible_v = find_deg_ge3_vertices graph
  in case all_possible_v of
    [] -> trace ("why do we have nothing in B2???\n" ++ show graph ++"\n"++show (connected_components graph)++show(is_co_path graph) ) Nothing
    _ ->
     let chosen_v = head all_possible_v
         degree_v = vertexDegree graph chosen_v
         deletion_branches = find_subset chosen_v Nothing (degree_v - 2) graph
     in Just (degree_v, deletion_branches)

-- helper for is_path_component. Given a tuple of (count of degree 1 vertices, count of degree 2 vertices)
-- update the tuple according to the input
update_count_tuple :: Maybe (Int, Int) -> Int -> Maybe (Int, Int)
update_count_tuple Nothing _ = Nothing
update_count_tuple (Just (deg1, deg2)) curr_degree =
  case curr_degree of
    1 -> Just (deg1 + 1, deg2)
    2 -> Just (deg1, deg2 + 1)
    _ -> Nothing


-- given a component, checks if the component is a path graph
is_path_component :: [Int] -> UGraph Int () -> Bool
is_path_component [] _ = True
is_path_component [singleton] _ = True

-- The component is a path if all vertices except 2 have degree 2 and the 2 terminal vertices have degree 1
-- , assuming component has size >1
-- Source: https://en.wikipedia.org/wiki/Path_graph
is_path_component component graph =
  let degrees = foldl (\degree_count vertex -> update_count_tuple degree_count (vertexDegree graph vertex))
                        (Just (0, 0)) component
  in case degrees of
      Nothing -> False
      Just (deg1, deg2) -> deg1 == 2 && deg2 == (length component) - 2

-- helper for is_co_path by checking if every component is a path graph
is_co_path_helper :: [[Int]] -> UGraph Int () -> Bool
is_co_path_helper [] _ = True
is_co_path_helper (curr : rest) graph =
  (is_path_component curr graph) && (is_co_path_helper rest graph)

-- checks if the graph is already co-path
is_co_path :: UGraph Int () -> Bool
is_co_path graph =
  let components = connected_components graph
  in is_co_path_helper components graph

-- branch according to the given branching list (in B1 and B2), output the first result that worked
branching :: UGraph Int () -> [[Edge Int ()]] -> Int -> [Edge Int ()] -> Maybe [Edge Int ()]
branching graph delete_set_branches new_k prev_delete_set =
  let branch_result = map (\delete_set -> find_co_path_set (removeEdges delete_set graph) (delete_set ++ prev_delete_set) new_k) delete_set_branches
      branches_succeed = filter (\result -> result /= Nothing) branch_result
  in case branches_succeed of
        [] -> Nothing
        result -> head result

-- find a set of at most k edges whose removal from the graph results in a graph in which every connected component
-- is a path
find_co_path_set :: UGraph Int () -> [Edge Int ()] -> Int -> Maybe [Edge Int ()]
find_co_path_set graph prev_delete_set k =
  if is_co_path graph && k >= 0
    then Just prev_delete_set
    else
      case (checkR1 graph, checkR2 graph, checkB1 graph, checkB2 graph, k <= 0) of
        (_,_,_,_, True) -> Nothing
        (Just (new_graph, delete_set), _,_,_,_) -> find_co_path_set new_graph (prev_delete_set ++ delete_set) (k-1)
        (_,Just (new_graph, delete_set), _,_,_) -> find_co_path_set new_graph (prev_delete_set ++ delete_set) (k - (length delete_set))
        (_,_,Just (degree_v, delete_set_branches),_,_) -> branching graph delete_set_branches (k - degree_v + 2) prev_delete_set
        (_,_,_,Just (degree_v, delete_set_branches),_) -> branching graph delete_set_branches (k - degree_v + 2) prev_delete_set
--        _ -> trace ("not matching any cases!\n"++)

-- pretty print edges
print_edges :: [Edge Vertex ()] -> IO()
print_edges [] = pure ()
print_edges ((Edge from to ()) : t) = putStrLn (show from ++ " -> " ++ show to) >> print_edges t

-- run the algorithm given a graph and k
run_algorithm :: UGraph Int () -> Int -> IO ()
run_algorithm graph k = do
  plotUGraph graph
  let graph_running = removeVertices (isolatedVertices graph) graph -- isolated ones are ignored
  case (find_co_path_set graph_running [] k) of
    Just delete_set -> do
      plotUGraph (removeEdges delete_set graph)
      putStrLn (show (length delete_set) ++ " edges are deleted. They are:")
      print_edges delete_set
    Nothing -> putStrLn "Not possible"

main :: IO ()
main =
  getArgs >>= go
  where
    go :: [String] -> IO ()

    go ["--random", probability_str, num_vertices_str, k_str] = do
      let probability = read probability_str :: Float
          num_vertices = read num_vertices_str :: Int
          k = read k_str :: Int
      graph <- rndGraph' probability [1..num_vertices]
      run_algorithm graph k

    go ["--csv", filename, k_str] = do
      graph <- fromCsv' filename
      let k = read k_str :: Int
      run_algorithm graph k

    go [filename, k_str] = do
      file <- readFile filename
      let graph = parse_graph file
          k = read k_str :: Int
      run_algorithm graph k

    go _ = putStrLn "Error input. Usage: ./co-path --random [edge probability] [num vertices] [k] \nOR: ./co-path [filename] [k] \nOR: ./co-path --csv [csv filename] [k]"
