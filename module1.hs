module module1 where
import Data.List
import Data.Char


{-
-}

--判断第一个列表是否包含于第二个列表之中
{-
比如，列表[3,4]包含于列表[1,2,3,4,5]之中，而未包含于列表[2,5]中
我们将带搜索的列表称作干草堆(haystack)，而将搜索的对象称之为缝纫针(needle)

tail函数是Data.List中的一员
Extract the elements after the head of a list, which must be non-empty.
>>> tail [1, 2, 3]
[2,3]

tails需要Data.List
tails "party"
["party","arty","rty","ty","y",""]

isPrefixOf :: Eq a => [a] -> [a] -> Bool

O(min(m,n)). The isPrefixOf function takes two lists and returns True iff the first list is a prefix of the second.

>>> "Hello" `isPrefixOf` "Hello World!"
True
>>> "Hello" `isPrefixOf` "Wello Horld!"
False

告诉我们第二个列表是否以第一个列表开头

any -- 取一个限制条件和一个列表作为参数，告诉我们是否列表中存在满足该条件的元素

any (> 4) [1,2,3]
False

any (=='F') "Frank sobotka"
True
-}
isIn :: (Eq a) => [a] -> [a] -> Bool
x `isIn` y = any (x `isPrefixOf`) (tails y)

{-
"art" `isIn` "party"
True
-}

{-
凯撒密码沙拉
Data.Char

ord 'a'
97
chr 97
'a'
ord 'a'返回97是因为'a'是Unicode编码表中第97个字符

加密字符 -- page 78
-}