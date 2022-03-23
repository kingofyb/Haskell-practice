module curry where
{-
截断 section
将一个参数放在中缀函数的一侧，并在外面用括号括起，即可截断这个中缀函数
最终得到一个一元函数
其参数代表中缀函数剩余的参数
-}

--下面是一个简单的例子
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

{-
divideByTen 200 与调用 (/10) 200 或者调用 200/10 是等价的
如果使用负号或者减号运算符
使用subtract
-}

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{-
第一个参数是一个取两个参数并返回一个值的函数
其中两个参数的类型不必相同
第二、三歌参数都是列表
返回值也是一个列表
-}

flip' :: (a -> b -> c) -> b -> a -> c 
flip' f y x = f x y

{-
(a -> b -> c) -> (b -> a -> c) 与 (a -> b -> c) -> (b -> (a -> c))等价
只要调用flip' f 而不带y和x两个参数，就能够返回一个参数顺序颠倒过的f
-}

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter p (x:xs)
   | p x = x : filter p xs
   | otherwise = filter p xs
   
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
   let smallerOrEqual = filter (<= x) xs
      larger = filter (>x) xs
   in quicksort smallerOrEqual ++ [x] ++ quicksort larger
   
{-
## 克拉兹序列 Collatz sequence
从任意自然数开始：
如果是1，停止；
如果是偶数，将它除以2；
如果是奇数，将它除以3然后加1；
取所得的结果，重复上述算法
数学家推论说，无论从任何数开始，最后都得1

##问题：分别以1到100之间的所有数作为起始数，有多少克拉兹链的长度大于15？
-}

chain :: Integer -> [Integer]
chian 1 = [1]
chain n
   | even n = n : chain (n 'div' 2)
   | odd n = n : chain (n*3 + 1)
-- 克拉兹链会止于1

-- 1-100 的结果
numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
   where isLong xs = length xs > 15
   
   
{-
## lambda

要声明一个lambda就写一个\
因为它看起来像是希腊字母的lambda
后跟函数的参数列表，
参数之间用空格分隔，
->后面是函数体
-}

-- 比如
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- \xs -> length xs > 15 返回一个函数，它可以告诉我们一个列表的长度是否大于15 

flip'' :: (a -> b -> c) -> b -> a -> c 
flip'' f = \x y -> f y x


{-
foldl 左折叠
-}

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs --从0开始加

{-
\acc x -> acc + x是一个二元函数
0是初始值，xs是待折叠的列表
一开始累加值为0
当前项为3，调用二元函数进行简单的加法计算得3，作为新的累加值
如果 sum' [3,5,2,1]
1.
acc 0
xs [3,5,2,1]
2.
acc 0+3
xs [3,5,2,1]
3.
acc 3+5
xs [5,2,1]
.
.
.
-}

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 -- (\acc x -> acc + x)与 (+) 等价

{-
foldr的行为与foldl相似，不过累加值是从列表的右端开始
右折叠的二元函数的参数顺序也是相反的
第一个参数是当前列表值
第二个参数是累加值

左折叠无法处理无限列表，右折叠可以

生成新列表，一般使用右折叠，:比++效率更高
-}

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


{-
假如有个二元函数f，初始累加值z，如果对列表[3,4,5,6]进行右折叠
f 3 (f 4 (f 5 (f 6 z)))
f会以列表的最后一个元素和累加值来调用
f = +, z = 0
3 + (4 + (5 + (6 + 0)))

当使用scanl时，最终结果是结果列表的最后一个元素
scanr中则是结果列表的第一个元素
-}

-- 将自然数的平方根依次相加，会在何处超过1000？
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanll (+) (map sqrt [1..]))) + 1

{-
$函数，也叫函数应用符 function application operator
($) :: (a -> b) -> a -> b
f $ x = f x
普通的函数（用空格隔开）有最高的优先级，而$的优先级最低
用空格的函数调用符是左结合，而$则是右结合
比如
sum (map sqrt [1..130])
由于$的低优先级，可以改成
sum $ map sqrt [1..130]
这样一来$右侧的表达式就作为参数应用到左侧函数上了
可以将$看做是在表达式右侧写一对括号的等价形式
f $ g $ x 与 f $ (g $ x)等价
map ($ 3) [(4+),(10*),(^2)]
[7.0,30,9]
列表中每个函数都应用到了3上
-}