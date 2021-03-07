import Data.List -- It must be imported to use list functions

-- Generating a List
asc:: Int -> Int -> [Int]
asc n m 
    | m < n = []
    | m == n = [m]
    | m > n = n : asc (n+1) m

generateBooleanList:: Int -> Bool  ->  [Bool]
generateBooleanList n value 
    | n == 1 = []
    | n > 1 = value : generateBooleanList (n-1) value 
    
list1 = generateBooleanList 10 True  

list2 = generateBooleanList 10 False

x = asc 10 20 -- Assigment result of function to variable

-- List comprehension
result = [ (2 * y, 10 * z) | y <- x , z <- x ]

-- List Patterns
-- Example 1
mysum :: [Int] -> Int 
mysum [] = 0
mysum (t:ts) = t + mysum ts 

-- Example 2
evens:: [Int] -> [Int]
evens [] = []
evens (f:fs) 
    | mod f 2 == 0 = f : evens fs
    | otherwise  = evens fs

-- Tuples
myfst :: (a,b) -> a
myfst (x, _) = x

mysnd:: (a,b) -> b
mysnd (_, y) = y

-- List of Tuples 
-- Sum of tuple list.
addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x + y | (x,y)  <- xs]

--- Exercise 1
checkElement:: (Eq a) =>  [a]-> a -> Bool
checkElement [] elem = False 
checkElement (x:xs) elem  
                        | x == elem = True 
                        | otherwise = checkElement xs elem   

-- Exercise 2
mynub :: (Eq a) => [a] -> [a]
mynub [] = []
mynub (b : bs)  
            | checkElement bs b = mynub bs
            | otherwise   = b :  mynub bs

-- Aternative with list function
mynub2 :: (Eq a) => [a] -> [a]
mynub2 [] = []
mynub2 (b : bs)  
            |  b `elem` bs = mynub2 bs
            | otherwise   = b :  mynub2 bs


-- Exercise 3
isAsc:: [Int] -> Bool 
isAsc [] = True 
isAsc [x] = True 
isAsc (x:y:xs) = (x <= y) && isAsc (y:xs) -- Adding y again xs 


-- Exercise 4 -- TODO
hasPath :: [(Int,Int)] -> Int -> Int -> Bool 
hasPath [] x y = x == y 
hasPath xs x y 
    | x == y = True 
    | otherwise =
        let xs' = [(n,m) | (n,m) <- xs, n /= x ]  
        in  or [ hasPath xs' m y | (n, m) <- xs, n == x] --- It creates  a list of boolean and we applied or operation on the
                                                         --- If 
        


main = do
     print $ asc 10 20 -- Generating list
     print $ head x -- Getting head of list
     print $ tail x  -- Getting tail of list
     print $ length x -- Getting lenght of list
     print $ init x -- Getting list with last element removed as copy.
     print $ null [] -- Checking if the list empty 
     print $ null x
     --- Functions on Lists of Booleans
     print $ generateBooleanList 10 True
     print list1
     print result -- Function application
     print $ mysum [10,20,33,112,33] -- Getting sum of list
     print $ evens [10, 30, 22, 33,11,17] -- Getting evens
     print $ myfst (10,20) -- Getting first element
     print $ mysnd (20,30) -- Getting second element
     print $ addTuples [(10,30), (10,50), (1,6), ( 20, 20)] -- Sum of list of tuple
     print $ checkElement [1,40,52,3] 1
     print $ checkElement [1,30,42] 10
     print $ mynub [10,10,22,11,22,343,312,33,22]
     print $ mynub2 [10,10,22,11,22,343,312,33,22]
     print $ isAsc [10,30,43]
     print $ isAsc [10,30,43,1]
     print $ hasPath [(1,2),(2,3),(3,4),(3,2),(4,5)] 1 5
    -- 1 -> 2 -> 3 -> 4  -> 5
    --       ^_ /
    

    

