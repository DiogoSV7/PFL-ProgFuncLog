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
cities :: [(String, String, a)] -> [String]
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


-- NAOOOOO ESTAAAA TESTADAAAA PARA EFICIENCIA COMO DIZ NO FINAL DO PROJETO

shortestPath :: RoadMap -> City -> City -> (Int, [Path])
shortestPath roadMap start end
    | start == end = (0, [[start]]) -- Shortest path from a city to itself
    | otherwise = 
        let allPaths = dfsPaths roadMap start end [] 0 -- Get all paths with accumulated distance
            distances = map snd allPaths -- Extract distances from paths
            minDistance = minimum distances -- Find the minimum distance
            shortestPaths = [path | (path, dist) <- allPaths, dist == minDistance] -- Filter paths with min distance
        in (minDistance, shortestPaths)

dfsPaths :: RoadMap -> City -> City -> [City] -> Int -> [(Path, Int)]
dfsPaths roadMap current end visited currentDistance
    | current `elem` visited = [] -- Avoid cycles
    | current == end = [([end], currentDistance)] -- Found a path to the end city
    | otherwise =
        let newVisited = current : visited
            neighbors = adjacent roadMap current
            paths = [dfsPaths roadMap neighbor end newVisited (currentDistance + d) | (neighbor, d) <- neighbors]
        in concat paths >>= \(path, distance) -> return (current : path, distance)


travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]