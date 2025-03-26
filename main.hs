module Main where

-- 1
myLast :: [a] -> Maybe a
myLast []     = Nothing
myLast [x]    = Just x
myLast (_:tl) = myLast tl

-- 2
myButLast :: [a] -> Maybe a
myButLast []     = Nothing
myButLast [x,_]  = Just x
myButLast (_:tl) = myButLast tl

-- 3
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (hd:tl) n
    | n == 1    = Just hd
    | otherwise = elementAt tl (n-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:tl) = 1 + myLength tl

-- 5
myReverse :: [a] -> [a]
myReverse xs =
    let aux acc xs =
            case xs of
            [] -> acc
            hd:tl -> aux (hd:acc) tl
    in aux [] xs

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a
    = Elem a
    | List [NestedList a]
    deriving (Eq, Show)

-- 7
flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = concatMap flatten xs

-- 8
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

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = dups : pack tl
    where (dups, tl) = extract xs

extract :: Eq a => [a] -> ([a], [a])
extract (fst : tl@(snd : _))
    | fst == snd =
        let (dups, rem) = extract tl in (fst:dups, rem)
    | otherwise = ([fst], tl)
extract x = (x, [])

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs =
    let aux acc xs = case xs of
            []    -> acc
            hd:tl -> aux ((length hd, head hd) : acc) tl
    in reverse $ aux [] $ pack xs

-- 11

data Encoding a
    = Single a
    | Multiple Int a
    deriving Show

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified xs =
    let aux acc xs = case xs of
            []    -> acc
            hd:tl ->
                let len = length hd in
                let new = if len == 1 then
                        Single (head hd)
                    else
                        Multiple len (head hd)
                in aux (new : acc) tl
    in reverse $ aux [] $ pack xs

main = return ()
