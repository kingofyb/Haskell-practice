module IO-operation where

{-
main = putStrLn "hello, world"
putStrLn取一个字符串为参数，返回一个I/O操作，而I/O操作的返回类型为()，空元组也成为单元(unit)
空元组的值为(),类型也为()
当我们将其命名为main并运行程序时，I/O操作就会被执行
-}

main = do
   putStrLn "Hello, what's your name?"
   name <- getLine
   putStrLn ("Hey " ++ name ++ ", you rock!")
   
{-
每个步骤都是一个I/O操作。 I/O()
main的类型签名总是 main :: IO something
其中something是一个具体类型

执行I/O操作getLine，然后将它的返回值绑定到name
getLine的类型为IO String，可知name的类型为String
-}


-- 几个实用的I/O函数

-- putStr
{-
跟putStrLn相似，但是
putStrLn会在显示完成之后换行，而putStr不会
-}

-- putChar

{-
putChar函数取一个字符作为参数，返回一个在终端显示字符的I/O操作

基于putChar，我们能够递归地定义出putStr
putStr的基准条件为空字符串，如果试图打印空字符串，那么就简单地返回一个什么都不做的I/O操作
即return()
如果不为空，则通过putChar显示字符串的首个字符，然后递归地显示剩余部分

例如：
putStr :: String -> IO ()
putStr [] = return []
putStr (x:xs) = do
   putChar x
   putStr xs
-}

-- print

{-
print取一个show的实例类型的值作为参数，对它应用show
相当于putStrLn . show
先对值应用show，然后将得到的结果交给putStrLn，返回一个将值输出的I/O操作

-}

-- when

{-
下面的小程序从终端读取输入，如果输入为SWORDFISH，则将输入的内容原样输出

import Control.Monad

main = do
   input <- getLine
   when (input == "SWORDFISH") $ do
      putStrLn input
	  
如果没有when，只能这么写
main = do
   input <- getLine
   if (input == "SWORDFISH)
      then putStrLn input
	  else return ()

可见，when在这样的情景中很有用：
我们希望在条件满足时执行一些I/O操作，在条件不满足时什么也不做
-}

-- sequence

{-
sequence 函数取一组I/O操作组成的列表作为参数，返回一个I/O操作
将列表中的I/O操作依次执行
最后的返回值为列表中所有I/O操作执行后的结果组成的列表
例如可以把一下程序
main = do
   a <- getLine
   b <- getLine
   c <- getLine
   print [a,b,c]
   

改成
main = do
   rs <- sequence [getLine, getLine, getLine]
   print rs
-}

-- mapM

{-
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]

ghci> mapM_ print [1,2,3]
1
2
3
-}


-- forever

{-
forever函数取一个I/O操作作为参数，返回一个永远重复执行该I/O操作的I/O操作
以下面的小程序为例，它会不停地等待用户输入，并将输入转换为大写字符输出
import Control.Monad
import Data.Char

main = forever $ do
   putStr "Give me some input: "
   l <- getLine
   putStrLn $ map toUpper l
-}

-- forM

{-
与mapM相似，不过它们的参数的顺序是相反的
forM的第一个参数是列表，第二个参数是映射到列表上的函数，最后将I/O操作排为序列
import Control.Monad
main = do
   colors <- forM [1,2,3,4] (\a -> do
      putStrLn $ "Which color do you associate with the number "
	     ++ show a ++ "?"
	  color <- getLine
	  return color)
   putStrLn "The colors that you associate with 1, 2, 3, 4 are: "
   mapM putStrLn colors
   
结果：
Which color do you associate with the number 1?
red
Which color do you associate with the number 2?
blue
Which color do you associate with the number 3?
black
Which color do you associate with the number 4?
grey
The colors that you associate with 1, 2, 3, 4 are: 
red
blue
black
grey

注意其中的lambda (\a -> do ...)是一个取数字作为参数返回I/O操作的函数
在do代码块中我们调用了return color, 
执行 color <- getLine 然后再return color 相当于将getLine的结果解开再重新包装
与直接调用getLine的效果完全相同
forM函数在接到两个参数之后产生一个I/o操作，我们将该I/O操作的结果绑定为colors，
而它只是一个普通的字符串组成的列表。
到最后，我们使用 mapM putStrLn colors 将所有颜色输出
-}