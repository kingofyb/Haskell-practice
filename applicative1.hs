import applicative1 where
import Data.Char
import Data.List
{-
applicative 函子
fmap :: (a -> b) -> f a -> f b
意思是：给我一个取参数a，返回b的函数，以及一个装有一个或几个a的盒子
我会给你一个盒子，里面有一个或几个b
它会把函数应用到容器里的元素上
-}

{-
如果你要对函子里的数据应用多个函数，请在代码顶层声明你自己的函数，创建一个lambda函数，
或者更理想的方式，使用函数组合(composition)
-}

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
		  

{-
如果输入hello there，将得到以下结果
$ ./fmapping_io
hello there 
E-R-E-H-T- -O-L-L-E-H

curry
(\xs -> intersperse '-' (reverse (map toUpper xs)))
-}

-- 11.1.2 作为函子的函数

{-
r -> a 可以改写为 (->) r a 
就像可以把 2 + 3 写成 (+) 2 3 一样
->是一个取两个类型参数的类型构造器，有点像Either

fmap :: (a -> b) -> f a -> f b
把每个f替换成我们的Functor实例，即(->) r 
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)

fmap :: (a -> b) -> (r -> a) -> (r -> b)
它接受从a到b的函数和从r到a的函数，返回从r到b的函数
相当于让r -> a 的输出变成 a -> b 的输入，得到函数 r -> b

所有的Haskell函数实际只接受一个参数。
函数 a -> b -> c 取一个类型为a的参数，返回函数 b -> c 
-}

-- 函子定律 functor law

-- 定律 1

{-
fmap id = id

instance Functor Maybe where
   fmap f (Just x) = Just (f x)
   fmap f Nothing = Nothing
-}

-- 定律 2

{-
fmap (f . g) = fmap f . fmap g
或
fmap (f . g) x = fmap f (fmap g x)
-}

-- 11.3 applicative 函子

{-
调用 fmap (*) (Just 3)
得到 fmap ((*) 3)
如果使用截断，这个表达式也可以写作
Just (3*) - 得到了一个包裹在Just里的函数 

ghci> :t fmap (++) (Jusr "hey")
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
-}

-- 向applicative 问好

{-
在Control.Applicative模块里我们可以见到Applicative类型类，这个类型定义了两个函数
class (Functor f) => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

第一行开始Applicative类的定义，同时引入了一个类约束
这个类约束说：如果我们想把某个类型构造器变成Applicative类型类的实例，它需要先成为Functor的实例
因此，如果我们知道一个类型构造器是Applicative类型类的实例，它必然也必是Functor的实例

第一个方法叫做pure
它的类型声明是 pure :: a -> f a 
f扮演的角色就是我们的applicative函子实例。
pure是接受任意类型值并返回一个包裹了该值的applicative值的一元函数

可以把 <*> 函数想象成某种加强的fmap

fmap :: (a -> b) -> f a -> f b
然而fmap接受一个函数和一个函子值，把函数应用到函子值的里面

(<*>) :: f (a -> b) -> f a -> f b
<*> 接受一个里面是函数的函子值和另一个函子
从第一个函子里取出函数然后映射到第二个函子里面的值

如果在applicative的上下文通过<*>处理Maybe，那就用pure，否则用Just

ghci> pure (+) <*> Just 3 <*> Just 5
Just 8

<*>是左结合，所以上面代码和下面是等价的
(pure (+) <*> Just 3) <*> Just 5
首先，+函数被放在一个applicative值里 —— 在这个例子里，是一个包含这个函数的Maybe值
所以我们有了 pure (+)，也就是 Just (+)
接着 Just (+) <*> Just 3 执行了，因为是部分应用，所以结果是 Just (3+) ，最后
Just (3+) <*> Just 5 被执行，结果是 Just 8

pure f <*> x 等同于 fmap f x 

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

如果我们想把函数f映射到三个applicative值上，可以写成
f <$> x <*> y <*> z
如果参数是普通值而非applicative函子的话，可以写成f x y z

ghci> (++) <$> Just "john" <*> Just "wright"
Just "johnwright"
-}

-- 列表
{-
instance Applicative [] where
   pure x = [x]
   fs <*> xs = [f x | f <- fs, x <- xs]
   
ghci> [(*0), (+100), (^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]

ghci> [(+),(*)] <*> [1,2] <*> [3,4]

[(1+),(2+),(1*),(2*)]

[(1+),(2+),(1*),(2*)] <*> [3,4]

[4,5,5,6,3,4,6,8]

>ghci [x*y | x <- [2,5,10], y <- [8,10,11]]

也可以用applicative风格

ghci> (*) <$> [2,5,10] <*> [8,10,11]
pure f <*> xs 等于fmap f xs 
pure f 就是[f], [f] <*> xs 把左边的每个函数应用到右边的每个值上
-}

-- IO

{-
main = do
   a <- (++) <$> getLine <*> getLine
   putStrLn $ "The two lines concatenated turn out to be: " ++ a
-}

-- 函数作为applicative

{-
instance Applicative ((->) r) where
   pure x = (\_ -> x)
   f <*> g = \x -> f x (g x)


当我们用pure把一个值包裹到applicative值里时，它产生的结果还是那个值
最小的默认上下文把那个值产生出来作为结果

ghci> (+) <$> (+3) <*> (*100) $ 5
508

当执行时，创建了一个函数，把+用在 (+3) 和 (*100) 的结果上，然后返回
(+3) 和 (*100) 先被应用到5上，返回8和500，然后+以这两个值作为参数被调用，返回508

ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
-}

-- applicative定律

{-
pure f <*> x = fmap f x 

pure id <*> v = v

pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

pure f <*> pure x = pure (f x)

u <*> pure y = pure ($ y) <*> u
-]

-- 11.4 applicative的实用函数

{-
Control.Applicative定义了一个名为liftA2的函数
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c 
liftA2 f a b = f <$> a <*> b

可以看成(a -> b -> c) -> (f a -> f b -> f c)

如果我们想把 Just 3 和 Just 4 变成 Just [3,4]
ghci> fmap (\x -> [x]) (Just 4)

现在有了Just 3 和 Just [4]
ghci> liftA2 (:) (Just 3) (Just [4])
Just [3,4]

或
ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]

: 取一个元素和一个列表，返回一个新列表，元素被添加到旧列表头部

实现一个这样的函数：接受applicative值的列表，返回以列表为结果的applicative值
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA [Just 1, Just 2]

(:) <$> Just 1 <*> sequenceA [Just 2]

(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])
sequenceA [] 就是 Just []
(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])

(:) <$> Just 1 <*> Just [2]

Just [1,2]

and 接受Bool的列表，如果列表元素全为True就返回True
-}
