-- Define bog function
bog :: (a -> [b]) -> [a] -> [b]
bog = concat . map

-- Define zog function
zog :: a -> [a -> b] -> [b]
zog = flip (concat . map)

-- Test bog function
bogExample :: [Int] -> [[Int]]
bogExample xs = bog (\x -> [x + 1, x + 2]) xs

-- Test zog function
zogExample :: [Int]
zogExample = zog 10 [\x -> x * 2, \x -> x * 3]

-- Test bog using hog
hog :: (c -> d) -> (a -> b -> c) -> a -> b -> d
hog = (.).(.)

bog' :: (a -> [b]) -> [a] -> [b]
bog' = hog (flip (++))

-- Verify results
bogExample [1, 2, 3]
zogExample
bog' (\x -> [x + 1, x + 2]) [1, 2, 3]
