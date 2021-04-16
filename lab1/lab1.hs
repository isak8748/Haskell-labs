preCompute :: [Int] -> Int -> [Int] -- Calculates the rolling sum, call with arg 0
preCompute [] _ = []
preCompute (x:xs) prev = [curr] ++ preCompute xs curr where curr = x + prev

insertS :: ((Int, Int), Int) -> Int -> [((Int, Int), Int)] -> [((Int, Int), Int)] --Inserts info about a sublist and maintains a sorted list based on sublist sum
insertS _ 0 _ = []
insertS t _ [] = [t]
--insertS ((start, end),sum) k (((cStart, cEnd),cSum):xs) = if sum <= cSum then [((start, end),sum)] ++ insertS ((cStart, cEnd),cSum) (k-1) xs else  [((cStart, cEnd),cSum)] ++ insertS ((start, end),sum) (k-1) xs
insertS ((start, end),sum) k (((cStart, cEnd),cSum):xs) 
        |sum <= cSum = [((start, end),sum)] ++ insertS ((cStart, cEnd),cSum) (k-1) xs
        |otherwise = [((cStart, cEnd),cSum)] ++ insertS ((start, end),sum) (k-1) xs

findIndices :: [Int] -> [Int] -> Int -> Int -> [((Int, Int), Int)] -> [((Int, Int), Int)] --Calls findIndices2 on with all start indices
findIndices [] _ _ _ l = l 
findIndices (x:xs) (pre:pres) k start l = findIndices xs pres k (start+1) (findIndices2 (pre:pres) k x pre start start l) 

findIndices2 :: [Int] -> Int -> Int -> Int -> Int ->  Int -> [((Int, Int), Int)] -> [((Int, Int), Int)] --Computes the tuple list for all sublist with a given start index
findIndices2 [] _ _ _ _ _ l = l
findIndices2 (x:xs) k startVal startSum start end l = findIndices2 xs k startVal startSum start (end+1) (insertS ((start, end), x-startSum+startVal) k l)

compute :: [Int] -> Int -> [((Int, Int), Int)] -- Calls findIndices to compute the final tuple list
compute [] _ = []
compute l k = findIndices l (preCompute l 0) k 1 []

subList :: Int -> Int -> Int -> [Int] -> [Int] --Returns the sublist with the specified index, starts with index 1
subList _ _ _ [] = []
subList start end count (x:xs) = curr ++ subList start end (count + 1) xs where curr = if count >= start && count <= end then [x] else []

solutionString :: [((Int, Int), Int)] -> [Int] -> String --All lines with content
solutionString [] _ = ""
solutionString (((start, end), size):xs) l = show size ++ "   " ++ show start ++ "   " ++ show end ++ "   " ++ (show (subList start end 1 l)) ++ "\n" ++ solutionString xs l 

printSolution :: [Int] -> Int -> String --Adds a line with headers
printSolution [] _ = error "List needs to be non-empty"
printSolution l k = "size i   j    sublist \n" ++ solutionString (compute l k) l 

solve l k = putStr (printSolution l k )

