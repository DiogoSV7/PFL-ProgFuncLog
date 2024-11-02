import qualified Data.List
import qualified Data.Array
import qualified Data.Bits


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]


-- | cities :: [(String, String, a)] -> [String]
-- | Given a road map represented as a list of tuples where each tuple contains two city names
-- | and an additional value (such as distance or cost), this function returns a list of 
-- | all unique city names involved in the road connections.
-- |
-- | Arguments:
-- | roadMap :: [(String, String, a)] - A list of tuples where:
-- |     * The first element (String) represents the name of the first city.
-- |     * The second element (String) represents the name of the second city.
-- |     * The third element represents the distance between the 2 cities.
-- |
-- | The function extracts all cities (both first and second city in each tuple) and 
-- | removes duplicates to return a list of distinct city names.
cities :: RoadMap -> [City]
cities roadMap = Data.List.nub [city | (c1, c2, _) <- roadMap, city <- [c1, c2]]


-- | areAdjacent :: RoadMap -> City -> City -> Bool
-- | This function checks if two cities are directly connected (adjacent) in the road map.
-- |
-- | Arguments:
-- | roadMap :: RoadMap - A list of tuples where each tuple represents a road between two cities.
-- |     * The first element (City) represents the first city in the connection.
-- |     * The second element (City) represents the second city in the connection.
-- |     * The third element represents the distance between the 2 cities.
-- | city1 :: City - The first city to check for adjacency.
-- | city2 :: City - The second city to check for adjacency.
-- |
-- | Returns 'True' if there is a direct connection (road) between the two cities, 
-- | regardless of the direction of the connection, otherwise returns 'False'.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = 
    any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap


-- | distance :: RoadMap -> City -> City -> Maybe Distance
-- | This function retrieves the distance between two cities, if they are directly connected in the road map.
-- | If the cities are connected by a road, the function returns the distance wrapped in 'Just'.
-- | If there is no direct connection, it returns 'Nothing'.
-- |
-- | Arguments:
-- | roadMap :: RoadMap - A list of tuples where each tuple represents a road between two cities.
-- |     * The first element (City) represents the first city in the connection.
-- |     * The second element (City) represents the second city in the connection.
-- |     * The third element represents the distance between the 2 cities.
-- | city1 :: City - The first city in the distance check.
-- | city2 :: City - The second city in the distance check.
-- |
-- | Returns 'Just Distance' if the two cities are directly connected by a road, otherwise returns 'Nothing'.
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = 
    case [d | (c1, c2, d) <- roadMap, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)] of
        []     -> Nothing
        (d:_)  -> Just d


-- | adjacent :: RoadMap -> City -> [(City, Distance)]
-- | This function retrieves a list of cities that are directly connected to a given city, along with their respective distances.
-- | The function returns a list of tuples, where each tuple contains an adjacent city and the distance to that city.
-- |
-- | Arguments:
-- | roadMap :: RoadMap - A list of tuples where each tuple represents a road between two cities.
-- |     * The first element (City) represents the first city in the connection.
-- |     * The second element (City) represents the second city in the connection.
-- |     * The third element represents the distance between the 2 cities.
-- | city :: City - The city for which adjacent cities and their distances are being retrieved.
-- |
-- | Returns a list of tuples, each containing an adjacent city and the distance to that city.
-- | If there are no adjacent cities, the function returns an empty list.
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadMap city = 
    [(c2, d) | (c1, c2, d) <- roadMap, c1 == city] ++ 
    [(c1, d) | (c1, c2, d) <- roadMap, c2 == city]


-- | pathDistance :: RoadMap -> Path -> Maybe Distance
-- | This function calculates the total distance of a given path through a series of cities.
-- | The path is represented as a list of cities, and the function checks the distances 
-- | between consecutive cities in the path as defined in the road map.
-- |
-- | Arguments:
-- | roadMap :: RoadMap - A list of tuples where each tuple represents a road between two cities.
-- |     * The first element (City) represents the first city in the connection.
-- |     * The second element (City) represents the second city in the connection.
-- |     * The third element (Distance) represents the distance between the two cities.
-- | path :: Path - A list of cities representing the path through which the total distance is calculated.
-- |
-- | Returns 'Just Distance' if the total distance can be calculated (i.e., all segments of the path are connected),
-- | or 'Nothing' if any segment of the path does not have a corresponding road in the road map.
-- |
-- | The function works recursively by checking the distance between the first two cities in the path,
-- | and if they are connected, it adds the distance to the total calculated for the remaining cities.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadMap (c1:c2:rest_of_the_array) = 
    case distance roadMap c1 c2 of
        Nothing -> Nothing
        Just d -> fmap (d +) (pathDistance roadMap (c2:rest_of_the_array))


-- | rome :: RoadMap -> [City]
-- | This function returns the names of the cities with the highest number of roads 
-- | connecting to them (i.e., the vertices with the highest degree) in the given road map.
-- |
-- | Arguments:
-- | roadMap :: RoadMap - A list of tuples where each tuple represents a road between two cities.
-- |     * The first element (City) represents the first city in the connection.
-- |     * The second element (City) represents the second city in the connection.
-- |     * The third element (Distance) represents the distance between the two cities.
-- |
-- | Returns a list of city names that have the highest number of connections in the road map.
rome :: RoadMap -> [City]
rome roadMap = 
    let 
        cityCounts = [(city, count) | city <- Data.List.nub (concatMap (\(c1, c2, _) -> [c1, c2]) roadMap),
                                      let count = length [() | (c1, c2, _) <- roadMap, c1 == city || c2 == city]]
        maxCount = maximum (map snd cityCounts)
    in  [city | (city, count) <- cityCounts, count == maxCount]


-- | isStronglyConnected :: RoadMap -> Bool
-- | This function checks if all the cities in the graph are connected,
-- | meaning that every city is reachable from every other city.
-- |
-- | It takes a road map as input and uses a depth-first search (DFS)
-- | starting from one city to determine if all cities can be reached.
-- |
-- | Arguments:
-- |   - roadMap: A list of tuples representing the roads between cities,
-- |               where each tuple contains two cities and the distance between them.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap =
    let allCities = cities roadMap
        startCity = head allCities -- Pick any city to start the search
        reachableCities = dfs roadMap startCity [] 
    in length reachableCities == length allCities


-- | dfs :: RoadMap -> City -> [City] -> [City]
-- | This helper function performs a depth-first search (DFS) to find all
-- | reachable cities from a given starting city.
-- |
-- | It takes a road map, a starting city, and a list of visited cities as input.
-- | The function returns a list of cities that can be reached from the starting city.
-- |
-- | Arguments:
-- |   - roadMap: A list of tuples representing the roads between cities,
-- |               where each tuple contains two cities and the distance between them.
-- |   - city: The city from which the DFS begins.
-- |   - visited: A list of cities that have already been visited during the search.
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap city visited
    | city `elem` visited = visited -- If already visited, return visited list
    | otherwise = foldr (\(adjCity, _) acc -> dfs roadMap adjCity acc) (city : visited) (adjacent roadMap city)


-- | shortestPath :: RoadMap -> City -> City -> [Path]
-- | This function finds all shortest paths (in terms of distance) between two cities in a road map.
-- | If the start city is the same as the end city, it returns a single path with that city.
-- |
-- | Arguments:
-- |   - roadMap :: RoadMap - A list of tuples representing the roads between cities.
-- |   - start :: City - The starting city for the path.
-- |   - end :: City - The destination city for the path.
-- |
-- | Returns:
-- |   - [Path]: A list of all shortest paths (each represented as a list of cities).
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end
    | start == end = [[start]]
    | otherwise =
        let allPaths = dfsPaths roadMap start end [] 0
            distances = map snd allPaths
        in if null distances
           then []
           else 
               let minDistance = minimum distances
               in [path | (path, dist) <- allPaths, dist == minDistance]


-- | dfsPaths :: RoadMap -> City -> City -> [City] -> Int -> [(Path, Int)]
-- | This function performs a depth-first search (DFS) to find all possible paths from the current city to the end city.
-- | It tracks the visited cities and the current distance traveled.
-- |
-- | Arguments:
-- |   - roadMap :: RoadMap - A list of tuples representing the roads between cities.
-- |   - current :: City - The city currently being explored.
-- |   - end :: City - The destination city for the path.
-- |   - visited :: [City] - A list of cities that have already been visited in the current path.
-- |   - currentDistance :: Int - The distance traveled so far.
-- |
-- | Returns:
-- |   - [(Path, Int)]: A list of tuples containing each found path and its corresponding distance.
dfsPaths :: RoadMap -> City -> City -> [City] -> Int -> [(Path, Int)]
dfsPaths roadMap current end visited currentDistance
    | current `elem` visited = []  -- Avoid revisiting cities
    | current == end = [([end], currentDistance)]
    | otherwise =
        let newVisited = current : visited
            neighbors = adjacent roadMap current
            paths = [dfsPaths roadMap neighbor end newVisited (currentDistance + d) | (neighbor, d) <- neighbors]
        in concat paths >>= \(path, distance) -> return (current : path, distance)


-- | travelSales :: RoadMap -> IO Path
-- | This function finds the best path for traveling sales from a specified starting city.
-- |
-- | Arguments:
-- |   - roadMap: A list of tuples representing the roads between cities, where each tuple 
-- |     consists of two cities and the distance between them.
-- |
-- | Returns an IO action that produces the best path found or raises an error if the 
-- | starting city is not valid or no valid paths are found.
travelSales :: RoadMap -> Path
travelSales roadMap = 
    let startCity = "0"  -- Specify the start city as "0"
        allCities = cities roadMap
    in if startCity `elem` allCities
       then let paths = tsp roadMap allCities [startCity] 0
                bestPathResult = bestPath roadMap paths
            in if null bestPathResult 
               then error "No valid paths found."
               else bestPathResult
       else error "Start city not in the list of cities."


-- | tsp :: RoadMap -> [City] -> Path -> Distance -> [Path]
-- | This function finds all possible paths for the Traveling Salesman Problem (TSP).
-- | It uses recursion to explore all permutations of cities, calculating the total distance
-- | for each completed path.
-- |
-- | Arguments:
-- |   - roadMap: A list of tuples representing the roads between cities.
-- |   - cities: A list of cities to visit.
-- |   - visited: A list of cities that have already been visited.
-- |   - currentDistance: The current distance traveled so far.
-- |
-- | Returns a list of completed paths that visit all cities and return to the starting city.
tsp :: RoadMap -> [City] -> Path -> Distance -> [Path]
tsp _ [] _ _ = []  -- No cities left to visit
tsp roadMap cities visited currentDistance
    | length visited == length cities = 
        let returnDistance = distance roadMap (last visited) (head visited)
        in case returnDistance of
            Nothing -> []
            Just d -> 
                let totalCost = currentDistance + d
                    completedPath = visited ++ [head visited]
                in completedPath : []
    | otherwise = 
        let unvisited = filter (`notElem` visited) cities
        in if null unvisited 
           then [] 
           else concat [tsp roadMap cities (visited ++ [nextCity]) (currentDistance + dist)
                        | nextCity <- unvisited, 
                          Just dist <- [distance roadMap (last visited) nextCity]]


-- | bestPath :: RoadMap -> [Path] -> Path
-- | This function finds the minimum cost path from a list of paths.
-- |
-- | Arguments:
-- |   - roadMap: A list of tuples representing the roads between cities.
-- |   - paths: A list of all possible paths.
-- |
-- | Returns the path with the smallest total distance. If no paths are given, it returns an empty list.
bestPath :: RoadMap -> [Path] -> Path
bestPath _ [] = []
bestPath roadMap paths = foldl1 shortest paths
  where
    shortest a b = if calculateTotalDistance roadMap a < calculateTotalDistance roadMap b then a else b


-- | calculateTotalDistance :: RoadMap -> Path -> Distance
-- | This function calculates the total distance of a given path.
-- |
-- | Arguments:
-- |   - roadMap: A list of tuples representing the roads between cities.
-- |   - path: A list of cities representing the path to evaluate.
-- |
-- | Returns the total distance traveled along the path. If the path is empty, it returns 0.
calculateTotalDistance :: RoadMap -> Path -> Distance
calculateTotalDistance _ [] = 0
calculateTotalDistance roadMap (x:xs) = sumDistances xs x
  where
    sumDistances [] _ = 0
    sumDistances (y:ys) prev = 
        case distance roadMap prev y of
            Nothing -> error "No path between cities." 
            Just d  -> d + sumDistances ys y



tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined 


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]