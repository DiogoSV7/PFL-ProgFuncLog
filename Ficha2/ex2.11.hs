pitagoricos::Integer -> [(Integer, Integer, Integer)]
pitagoricos x = [(a,b,c) | c <- [1..x],a <- [1..x], b <- [1..x], a^2 + b^2 == c^2 ]