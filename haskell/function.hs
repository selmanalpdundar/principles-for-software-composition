import Data.List

-- Defintion of function
inRange :: Integer -> Integer -> Integer -> Bool  -- function type definition
inRange min max x = x >= min &&  x <= max -- functiın definition

-- Let Binding
inRange2 :: Integer -> Integer -> Integer -> Bool 
inRange2 min max x = 
    let inLowerBound = min <= x
        inUpperBound = max >= x
    in
        inLowerBound && inUpperBound

-- Where Binding
inRange3:: Integer -> Integer -> Integer -> Bool 
inRange3 min max x = ilb && iub
    where
        ilb = min <= x
        iub = max >= x

main = do 
    print $ inRange 0 5 3 -- function application
    print $ inRange2 0 5 3 -- function application
    print $ inRange3 0 5 3 -- function application