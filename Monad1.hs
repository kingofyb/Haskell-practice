module Monad1 where
{-
(>>=) :: Monad m => m a -> (a -> m b) -> m b

有一个函数 \x -> Just (x+1)
它取一个数，加上1后把结果包裹在Just里
ghci> (\x -> Just (x+1)) 1
Just 2
-}

-- Monad 类型类
{-
class Monad m where
   return :: a -> m a

return跟Applicative里的pure一样
取一个值，把它放在能产生这个值最小默认上下文里
对Maybe来说，就是取一个值，包裹在Just里
这里的return并不会结果函数的执行

   (>>=) :: m a -> (a -> m b) -> m b

取一个monad值，喂给一个（取普通值，返回monad值的）函数


   (>>) :: m a -> m b -> m b
   x >> y = x >>= \_ -> y
   
   fail :: String -> m a
   fail msg = error msg
-}

{-
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left+n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right+n)

ghci> landLeft 2 (0, 0)
(2, 0)
ghci> landRight 1 (1, 2)
(1, 3)
ghci> landRight (-1) (1, 2)
(1, 1)
ghci> landLeft 2 (landRight 1 (landLeft 1 (0, 0))
(3, 1)

x := f = f x

ghci> 100 -: (*3)
300
ghci> True -: not
False
ghci> (0, 0) -: landLeft 2
(2, 0)

跟上面是等价的
ghci> (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2
(3, 1)

我们现在希望landLeft和landRight能返回失败信息，当保持平衡时，返回一个新的Pole，
否则返回失败信息
新的landLeft和landRight
landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left+n, right)
   | abs ((left + n) - right) < 4 = Just (left + n, right))
   | otherwise                    = Nothing

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right+n)
   | abs (left - (right + n)) < 4 = Just (left, right + n)
   | otherwise                    = Nothing

ghci> landLeft 2 (0, 0)
Just (2, 0)
ghci> landLeft 10 (0, 3)
Nothing

现在可以用 >>=, 把Maybe Pole 喂给一个取Pole返回Maybe Pole的函数
ghci> landRight 1 (0, 0) >>= landLeft 2
Just (2, 1)

LandLeft 2的类型是Pole -> Maybe Pole 我们不能把类型类Maybe Pole 的 landRight 1 (0, 0)喂给它
所以要使用 >>=
>>=让我们可以把Maybe值当作一个盒子

ghci> return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
Just (2,4)

一开始我们用return来接收一个Pole，把它包裹在Just里

   (>>) :: m a -> m b -> m b
   x >> y = x >>= \_ -> y

把某个值传递给一个总是忽略参数，返回预先决定好的monad值的函数，结果就会是那个决定好的值

ghci> Nothing >> Just 3
Nothing
ghci> Just 3 >> Just 4
Just 4
ghci> Just 3 >> Nothing
Nothing

foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
	  Just (show x ++ y)))
	 
	 
使用do记法可以改写成：
foo :: Maybe String
foo = do
   x <- Just 3
   y <- Just "!"
   Just (show x ++ y)

do表达式只是把monad值串起来的一套不同的语法罢了
在一个do表达式里，每一个不带let的行都是一个monad值
要获取它的结果就是使用 <-
比方说有一个Maybe String，我们用了 <- 把它绑定到某个变量
这个变量就会是一个String，就像我们用>>=把monad值喂给某个lambda一样

假设有两只鸟儿停在左边，然后有两只停在右边，然后有一个停在左边
routine :; Maybe Pole
routine = do
   start <- return (0, 0)
   first <- landLeft 2 start
   second <- landRight 2 first
   landLeft 1 second


ghci> routine
Just (3, 2)
-}

-- 列表Monad
{-
instance Monad [] where
   return x = [x]
   xs >>= f = concat (map f fs)
   fail _ = []
   
   
列表推导式
ghci> [(c,ch) | c <- [1,2] | ch <- ['a','b']]
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

我们对x应用show把数转换成字符串，然后检查字符'7'是否是这个字符串的一部分
ghci> [x | x <- [1..50], '7' `elem` show x]
[7,17,27,37,47]
-}

-- MonadPlus
{-
class Monad m => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a 
   
mzero跟Monoid的mempty一样，而mplus对应mappend
instance MonadPlus [] where
   mzero = []
   mplus = (++)
   
guard函数
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

guard取一个布尔值，如果为True就把()放在最小默认的盒子里返回
如果是False则产生一个表示计算失败的monad值

ghci> guard (5 > 2) :: Maybe ()
Just ()
ghci> guard (1 > 2) :: Maybe ()
Nothing
ghci> guard (5 > 2) :: [()]
[()]
ghci> guard (5 > 2) :: [()]
[]

ghci> [1..50] >>= (\x -> guard( '7' `elem` show x) >> return x)
[7,17,27,37,47]

ghci> guard (5 > 2) >> return "cool" :: [String]
["cool"]
ghci> guard (1 > 2) >> return "cool" :: [String]
[]
如果guard为True，它包含的就是一个空元组。所以我们用>>忽略空元组，把另一个东西作为结果
如果guard为False，就是立即失败
-}

-- 国际象棋 —— 旗子 马

type KnightPos = (Int, Int)

{-
假设马从6，2出发，它能三步到达6，1吗？
下面的函数取马的位置为参数，返回所有下一步可以到达的位置
-}
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do 
   (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
               (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
   guard (c' `elem` [1..8] && r' `elem` [1..8])
   return (c',r')

-- 或者不用monad
moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c,r) = filter onBoard 
              [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
               (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
   where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

-- 有了下一个位置，可以用 >>= 喂给moveKnight
-- 下面的函数取位置为参数，返回三步可以到达的所有位置
in3 :: KnightPos -> [KnightPos]
in3 start = do
   first <- moveKnight start
   second <- moveKnight first
   moveKnight second
   
-- 如果不用do
-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- 现在，创建一个函数，该函数取两个位置并告诉我们你是否能从其中一个位置三步到达另一个位置
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` In3 start

-- Monad定律
-- 左单位元
{-
return x >>= f 和 f x 一样
ghci> return 3 >>= (\x -> Just (x+100))
Just 103
ghci> (\x -> Just (x+100)) 3
Just 103
对于列表Monad，return把某个东西放在单元素列表里
列表的>>=实现遍历列表的所有元素，对它们应用那个函数
-}

-- 右单位元
{-
m >>= return 跟 m 没有差别
xs >>= f = concat (map f xs)
当我们把[1,2,3,4]喂给return时
首先return被映射到[1,2,3,4]上
得到[[1],[2],[3],[4]]
然后结果被拼接起来，得到原始列表
-}

-- 结合律
{-
(m >>= f) >>= g 应该和 m >>= (\x -> f x >>= g) 一样

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = (\x -> f (g x))

如果g的类型是(a -> b),f的类型是(b -> c)，我们就得到了类型为 a -> c 的另一个函数
如果函数的类型是 a -> m b，我们不能把它的结果传给 b -> c 因为它接收普通b
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)

ghci> let f x = [x,-x]
ghci> let g x = [x*3,x*2]
ghci> let h = f <=< g 
ghci> h 3
[9,-9,6,-6]

相当于就是 f <=< (g <=< h) 应该和 (f <=< g) <=< h 相同
-}