preCompute :: [Int] -> Int -> [Int] -- Calculates the rolling sum, call with arg 0
preCompute [] _ = []
preCompute (x:xs) prev = [curr] ++ preCompute xs curr where curr = x + prev

insertS :: ((Int, Int), Int) -> Int -> [((Int, Int), Int)] -> [((Int, Int), Int)] --Inserts info about a sublist and maintains a sorted list based on sublist sum
insertS _ 0 _ = []
insertS t _ [] = [t]
insertS ((start, end),sum) k (((cStart, cEnd),cSum):xs) = if sum <= cSum then [((start, end),sum)] ++ insertS ((cStart, cEnd),cSum) (k-1) xs else {--(((cStart, cEnd),cSum):xs)--} [((cStart, cEnd),cSum)] ++ insertS ((start, end),sum) (k-1) xs

findIndices :: [Int] -> [Int] -> Int -> Int -> [((Int, Int), Int)] -> [((Int, Int), Int)]
findIndices [] _ _ _ l = l 
findIndices (x:xs) (pre:pres) k start l = findIndices xs pres k (start+1) (findIndices2 (pre:pres) k x pre start start l) 

findIndices2 :: [Int] -> Int -> Int -> Int -> Int ->  Int -> [((Int, Int), Int)] -> [((Int, Int), Int)]
findIndices2 [] _ _ _ _ _ l = l
findIndices2 (x:xs) k startVal startSum start end l = findIndices2 xs k startVal startSum start (end+1) (insertS ((start, end), x-startSum+startVal) k l)

compute :: [Int] -> Int -> [((Int, Int), Int)]
compute [] _ = []
compute l k = findIndices l (preCompute l 0) k 1 []

