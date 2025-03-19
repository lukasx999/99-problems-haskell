module Main where

myLast :: [a] -> Maybe a
myLast []     = Nothing
myLast [x]    = Just x
myLast (_:tl) = myLast tl

myButLast :: [a] -> Maybe a
myButLast []     = Nothing
myButLast [x,_]  = Just x
myButLast (_:tl) = myButLast tl

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (hd:tl) n
    | n == 1    = Just hd
    | otherwise = elementAt tl (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:tl) = 1 + myLength tl

myReverse :: [a] -> [a]
myReverse xs =
    let aux acc xs =
            case xs of
            [] -> acc
            hd:tl -> aux (hd:acc) tl
    in aux [] xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a
    = Elem a
    | List [NestedList a]
    deriving (Eq, Show)

flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = concatMap flatten xs

compress_ :: Eq a => [a] -> [a]
compress_ xs =
    let aux acc xs =
            case xs of
            [] -> acc
            [x] -> x:acc
            fst:tl@(snd:_) ->
                if fst == snd then
                    aux acc tl
                else
                    aux (fst:acc) tl
    in reverse $ aux [] xs

compress :: Eq a => [a] -> [a]
compress (fst:tl@(snd:_))
    | fst == snd = compress tl
    | otherwise = fst : compress tl
compress x = x


main = do
    putStrLn "Hello"
