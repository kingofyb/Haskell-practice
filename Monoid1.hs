module Monoid1 where
import Data.Functor
import Data.Monoid
import qualified Data.Foldable as F
{-
newtype 比 data速度快
如果用data，程序运行时会有一个包裹和解开包裹的开销
但如果用newtype，Haskell知道你只是把一个现有类型包裹成一个新的，
因为你想让它的内部实现和现有类型相同，只是想换个不同的类型
newtype根据已有类型创建新类型时，只能有一个值构造器，一个字段
而data可以有多个值构造器，每个值构造器也可以有零个或多个字段
e.g.
data profession = Fighter | Archer | Accountant

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq,show)

ghci>CharList "This will be shown"
CharList {getCharList = "This will be shown"}
ghci>CharList "benny" == CharList "benny"
True
-}
{-
class Functor f where
   fmap :: (a -> b) -> f a -> f b
   
instance Functor Maybe where
   fmap :: (a -> b) -> Maybe a -> Maybe b 
-}
newtype Pair b a = Pair {getPair :: (a,b)}

instance Functor (Pair c) where
   fmap f (Pair (x,y)) = Pair (f x, y)
   
-- 关于newtype特性
{-
data CoolBool = CoolBool (getCoolBool :: Bool)
这里用data关键字定义了一个普通的代数数据类型。它有一个值构造器，具有一个类型为Bool的字段
让我们创建一个函数对CoolBool做模式匹配，不管CoolBool里面是True还是False，都返回"hello"

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
ghci> helloMe undefined
"*** Exception"

用data关键字定义的类型可以有多个值构造器
Haskell必须对这个值求值知道可以发现使用的值构造器位置
如果求值过程中牵涉到undefined，就会出现异常

data CoolBool = CoolBool (getCoolBool :: Bool)
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

ghci> helloMe undefined
"hello"

工作了，为什么呢？
使用newtype时，Haskell内部把新类型的值和旧类型的值用同一种方式表示，它并没有添加额外的容器，
只是记住了新的值具有不同的类型，因为newtype只有一个值构造器，且只有一个字段
对newtype值进行模式匹配不是从容器里取出东西（data是这样），二十转换值的类型
-}


-- type、newtype、data三者的对比

{-
type关键字用于创建类型别名，仅仅是给已知类型赋予一个新名字
比如 type IntList = [Int]
并没有创建值构造器，[Int]和IntList只是引用同一个类型的两种方式

newtype关键字用来将已知类型包裹为新类型
newtype CharList = CharList {getCharList :: [Char]}
我们不能用++来拼接一个CharList和一个类型为[Char]的列表，也不能用++来拼接两个CharList列表
因为++只对列表有效，而CharList类型并不是列表，尽管它包含了一个列表
我们可以把两个CharList转换成列表，用++拼接它们，最后转换回CharList

可以把newtype声明想象成只有一个值构造器和一个字段的data声明

data关键字用来创建你自己的数据类型
-}

-- 12.2关于monoid类型类

{-
class Monoid m where
   mempty :: m
   mappend :: m -> m -> m 
   mconcat :: [m] -> m 
   mconcat = foldr mappend mempty
   
Monoid类型类定义在Data.Monoid模块里

--------- mempty
mempty实际上不是一个真正的函数，因为它不接受参数，只是个多态常量

--------- mappend
它是一个二元函数，接受同一个类型的两个值，返回那个类型的另一个值
并不是一定叠加两个值，而是一个根据两个monoid值返回第三个值的二元函数

--------- mconcat
它取一个monoid值组成的列表，通过mappend将其中的元素相连，归约成一个值
它有一个默认的实现，把mempty作为初始值，从右边开始mappend折叠这个列表

当把一个类型变成monoid实例时，只定义mempty和mappend就足够了

-}

-- monoid 定律
{-
--- mempty `mappend` x = x
--- x `mappend` mempty = x
--- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

前两条规定mempty在mappend的定义中必须是单位元
第三条规定必须满足结合律，不管顺序如何，结果都必须相同
-}

-- 12.3.1 列表是monoid
{-
instance Monoid [a] where
   mempty = []
   mappend = (++)
   
这里用的是instance Monoid [a]，而不是instance Monoid []
因为Monoid要求实例是一个具体的类型



-}

{-
在import Data.Monoid模块里
newtype Product a = Product {getProduct :: a}
   deriving (Eq,Ord,Read,Show,Bounded)
   
instance Num a => Monoid (Product a) where
   mempty = Product 1
   Product x `mappend` Product y = Product (x * y)
   
ghci> getProduct $ Product 3 `mappend` Product 9
27
   
-}


-- 12.3.3Any和All
{-
Bool也可以以两种不同但是同等有效的方式成为monoid
第一种方式是用令表示逻辑或的函数||为二元函数，False作为单位元
两个参数有一个为True，结果就是True

newtype Any = Any {getAny :: Bool}
   deriving (Eq,Ord,Read,Show,Bounded)
   
instance Monoid Any where
   mempty = Any False 
   Any x `mappend` Any y = Any (x || y)
   
   
ghci> getAny $ Any True `mappend` Any False
True


All 就是把||换成&&
只有所有值都为True时，mappend的结果才为True
-}

-- 12.3.4 Ordering monoid

{-
ghci> 1 `compare` 2
LT
ghci> 2 `compare` 2
EQ
ghci> 3 `compare` 2
GT

instance Monoid Ordering where
   mempty = EQ
   LT `mappend` _ = LT
   EQ `mappend` y = y
   GT `mappend` _ = GT

当我们把两个Ordering值mappend到一起时，如果左边的值不是EQ就保留
否则就返回右边的值

比较两个字符串的长度，如果等长就比较字母序
-}
{-
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a
-}


-- 如果用monoid，就可以更加简单的写
{-
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)
					
-}

-- 现在拓展一下，把比较元音的个数作为第二重要的准则
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

-- 12.3.5 Maybe monoid
{-
当类型参数a为monoid时，有一种把Maybe a看作Monoid并据此实现mappend的方式：
Nothing是单位元，当两个参数都不是Nothing时，mappend把计算任务转交给下层a的mappend

instance Monoid a => Monoid (Maybe a) where
   mempty = Nothing 
   Nothing `mappend` m = m
   m `mappend` Nothing = m 
   Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
   
如果有一个值为Nothing，结果就是那个值
如果mappend两个Just值，Just的内容就会被mappend起来，结果重新包裹在Just里面
Just里面的值的类型也是monoid实例

ghci> Nothing `mappend` Just "andy"
Just "andy"
ghci> Just LT `mappend` Nothing
Just LT
ghci> Just (Sum 3) `mappend` Just (Sum 4)
Just (Sum {getSum = 7})

newtype First a = First {getFirst :: Maybe a}
   deriving (Eq,Ord,Read,Show)
   
instance Monoid (First a) where
   mempty = First Nothing
   First (Just x) `mappend` _ = First (Just x)
   First Nothing `mappend` x = x 
   
mempty只是一个包裹在First newtype构造器里的Nothing
如果mappend的第一个参数是Just值，那就忽略第二个参数
如果第一个参数是Nothing，就把第二个参数作为结果返回

ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
Just 'a'
ghci> getFirst $ First Nothing `mappend` First (Just 'b')
Just 'b'
ghci> getFirst $ First (Just 'a') `mappend` First Nothing
Just 'a'

如果有很多Maybe值，仅仅想知道其中是不是有Just时
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
Just 9
-}

-- monoid的折叠

{-
ghci> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
ghci> :t F.foldr
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b

F.foldr接受任何可以被折叠的类型
ghci> F.foldl (+) 2 (Just 9)
11
ghci> F.foldr (||) False (Just True)
True
-}