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

main = do
    putStrLn "Hello"
