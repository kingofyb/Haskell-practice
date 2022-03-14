module Q1to10 where
{-
List
https://wiki.haskell.org/99_questions/1_to_10
-}
--1.find the last element of a list
--找最后一个元素
myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' = head . reverse

myLast'' [] = error "No end for empty lists!"
myLast'' x = x !! (length x - 1)

--2.find the last but one element of a list
--找倒数第二个元素
myButLast :: [a] -> a
myButLast = last . init

myButLast'' = head . reverse . init
{-
(!!) :: [a] -> Int -> a 
>>> ['a', 'b', 'c'] !! 0
'a'
>>> ['a', 'b', 'c'] !! 2
'c'
List index (subscript) operator, starting from 0. 
It is an instance of the more general genericIndex, 
which takes an index of any integral type.

init :: [a] -> [a]
>>> init [1, 2, 3]
[1,2]
last :: [a] -> a
>>> last [1, 2, 3]
3
-}

--3. Find the K'th element of a list. 
--找第k个元素
{-
λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'
-}

elementAt :: [a] -> Int -> a
elementAt list i  = list !! (i-1)

elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1 = x 
elementAt' (_:xs) i = elementAt' xs (i-1)
elementAt' _ _ = error "Index out of bounds"


--4.Find the number of elements of a list. (length)
--找有多少个元素
myLength :: [a] -> Int 
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

{-
--Using foldl/foldr
myLength1 =  foldl (\n _ -> n + 1) 0
myLength2 =  foldr (\_ n -> n + 1) 0
myLength3 =  foldr (\_ -> (+1)) 0
myLength4 =  foldr ((+) . (const 1)) 0
myLength5 =  foldr (const (+1)) 0
myLength6 =  foldl (const . (+1)) 0
-}

--change each element into our list into a "1" and then add them all together.
myLength' :: [a] -> Int 
myLength' = sum . map (\_->1)


--5.Reverse a list.
--翻转列表
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (\a x -> x:a) []
{-
flip :: (a -> b -> c) -> b -> a -> c

flip f takes its (first) two arguments in the reverse order of f.

>>> flip (++) "hello" "world"
"worldhello"
-}


--6.Find out whether a list is a palindrome. 
--A palindrome can be read forward or backward; e.g. (x a m a x).
--找是不是翻转 e.g. [a b c b a]
isPalindrome,isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)


--7.Flatten a nested list structure.
--展开嵌套列表
{-
λ> flatten (Elem 5)
[5]
λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
λ> flatten (List [])
[]
-}
data NestedList a = Elem a | List [NestedList a]
 
flatten,flatten' :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x 

flatten' (Elem a) = [a]
flatten' (List (x:xs)) = flatten x ++ flatten (List xs)
flatten' (List []) = []
{-
Map a function over all the elements of a container and concatenate the resulting lists.
>>> concatMap (take 3) [[1..], [10..], [100..], [1000..]]
[1,2,3,10,11,12,100,101,102,1000,1001,1002]
>>> concatMap (take 3) (Just [1..])
[1,2,3]
-}


--8.Eliminate consecutive duplicates of list elements.
--λ> compress "aaaabccaadeeee"
--"abcade"
--找unique
--compress :: Eq a => [a] -> [a]
--compress = map head . group
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys

{-
group :: Eq a => [a] -> [[a]]Source#

The group function takes a list and returns a list of lists such that the concatenation of 
the result is equal to the argument. Moreover, each sublist in the result contains only equal elements.
>>> group "Mississippi"
["M","i","ss","i","ss","i","pp","i"]
-}


--9. Pack consecutive duplicates of list elements into sublists. 
--If a list contains repeated elements they should be placed in separacmdte sublists.
-- (pack '(a a a a b c c a a d e e e e))
--((A A A A) (B) (C C) (A A) (D) (E E E E))

pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack [] = []

{-
span :: (a -> Bool) -> [a] -> ([a], [a])
>>> span (< 3) [1,2,3,4,1,2,3,4]
([1,2],[3,4,1,2,3,4])
>>> span (< 9) [1,2,3]
([1,2,3],[])
>>> span (< 0) [1,2,3]
([],[1,2,3])
-}


--10.
--λ> encode "aaaabccaadeeee"
--[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
--encode xs = map (\x -> (length x,head x)) (group xs)
--encode xs = [(length x, head x) | x <- group xs]
{-
group老报错
-}
encode = map ((,) <$> length <*> head) . pack