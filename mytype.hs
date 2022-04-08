module module1 where

{-

-}

--定义类型
{-
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
   deriving (Show)
   
--若在data声明后面加上deriving (Show)，那么Haskell就会自动将该类型置于Show类型类之中
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs y2 - y1)
-}
--借助point类型优化shape
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2) = (abs $ x2 - x1) * (abs y2 - y1)

{-
area (Circle (Point 0 0) 24)
>>>1809.5574
-}

--表示移动一个图形的函数该怎么写？它应该取一个图形和分别表示x轴与y轴位移的两个数
--返回一个位于坐标系中新位置的新图形
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2) a b = Rectangle (Point (x1+a) (y1+b))
(Point (x2+a) (y2+b))

{-
nudge (Circle (Point 34 34) 10) 5 10
>>>Circle (Point 39.0 44.0) 10.0
-}


-- 记录语法 record syntax
data Person = Person ( firstName :: String
                     , lastName :: String
					 , age :: Int
					 , height :: Float
					 , phoneNumber :: String
					 , flavor :: String) deriving (Show)
					 
data Car = Car { company :: String
               , model :: String
			   , year :: Int
			   } deriving (Show)
{-
Car {company = "Ford", model = "Mustang", year = 1967}
>>>...
记录语法适用于构造器的字段较多且不容易区分的情况
如果是 Vector = Vector Int Int Int 就足够明白了
-}

--类型参数
data Maybe a = Nothing | Just a
{-
这里a就是一个类型参数，maybe是类型构造器
Nothing的类型是多态的 polymorphic 意味着它的实际类型随类型变量（即Maybe a中的a）而变化
-}

--deriving 派生实例
{-
data Person = Person ( firstName :: String
                     , lastName :: String
					 , age :: Int
					 } deriving (Eq)
如果有两条人的记录，有没有可能检查这两条记录是否是同一个人呢？
可以按照姓名和年龄的相等性来判断他们两个是否相等
输出要用show
-}

--升级电话本
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

{-
4 `Cons` (5 `Cons` Empty) 相当于 4:(5:[])
:本身是一个值构造器，会取一个值和另一个列表作为参数，返回一个新的列表
换句话说，它有两个字段，一个字段的类型为a，另一个字段的类型为List a
可以用 a :-: (List a) 替换 Cons a (List a)
-}

{-
如果有了一个类型类，该怎么使用呢？
-}
data TrafficLight = Red | Yellow | Green

--实例
instance Eq TrafficLight where
   Red == Red = True
   Green == Green = True
   Yellow == Yellow = True
   _ == _ =False

{-
class用于定义新的类型，而instance用于将类型转为某类型类的实例
TrafficLight扮演着Eq的实例类型角色
instance Eq a where, a必须是具体类型
Maybe只是一个类型构造器，必须取一个参数才能产生具体类型
比如(Maybe m) (Maybe m)扮演了class Eq a where 中a的角色
instance (Eq m) => Eq (Maybe m) where
在上面那段实例声明中，我们期望Maybe m形式的所有类型都属于Eq类型类
此外要求m所表示的类型同样类Eq类型类的实例
-}

instance Show TrafficLight where
   show Red = "Red light"
   show Yellow = "Yellow light"
   show green = "Green light"
   
   
{-
函数 f 接收 x 返回 f(x)
也可以写成
a -> b
-}

class Functior f where
   fmap :: (a -> b) -> f a -> f b

{-
这里的f不是具体类型，而是一个取一个类型参数构造器
Maybe Int是一个具体类型，Maybe是一个取一个类型参数的类型构造器
-}