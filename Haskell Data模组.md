﻿#Haskell Data模组

## Data.List


----------


- intersperse 取一个元素与list作参数，并将该元素置于List元素的中间
- intercalate 取两个list做参数。它会将第一个List交叉插入两个list中间，并返回新的list
- transpose 反转List的List，行变列，相当于转置
- concat 把一组List连接为一个List
- concatMap 先进行map后再concat
- and 取一组布尔值的List做参数，只有其中全为True的话，才为True
- or 与and相反，有一个True就返回True
- and 取一个条件和List，检查是否全部符合条件
- any 取值和all相同，检查是否有元素复合该条件
- iterate 去一个函数和一个值做参数。它会用值调用函数，得到结果再次调用函数，产生一个无限的List
- splitAt 取一个List和数值做参数，并在该数值位置断开，返回一个二元组。
- takeWhile 从一个List中取元素，不符合条件就停止
- dropWhile 它扔掉符合条件的元素，一旦返回Fasle就返回List余下的部分
- span 返回两个List第一个是List同参数调用takewhile所得的结果，第二个是剩下的部分
- break 是True是断开List，其余行为和span相同
- sort 排序
- group 取一个List做参数， 并将其中相邻并相等的元素各自归类，形成一个List
- inits 会递归的调用自身最后什么都不剩
- partition 去一个限制条件和List做参数，返回两个List。第一个中包含所有符合条件的List，第二个包含剩下的。这个会遍历整个List。
- find 去一个List和限制条件做参数，并返回首次符合条件的函数，而这个元素是个MayBe值。
- elemIndex maybe返回我们所寻找值得下标
- elemIndices 它返回的是个List，不存在list为空，其中返回所有相同元素的下标
- findIndex 与find相同不过返回的是元素的下标，也是返回首次
- findIndices 返回所有相同元素的下标
- zipWith3 与zipWith相同，不过支持3个参数的操作，直到zipWith7
- zip3 同上，直到7
- lines 以一个字符串为参数，返回其中每一行所组成的List
- unlines 与上面的相反，去一组的string的List，合并成一个string
- words 与 unwords 将一个字符串中所有的单词变成一个List，unwords相反
- nub 去除其中相同的元素
- delete取一个元素和List做参数，会删掉List中首先出现的元素
- \\ 表示List的差集操作 会除掉所有在右边在左边的元素
- union 相当于集合中的并集
- intersection 相当于集合的交集，他返回两个List中相同的部分
- insert 可以讲一个元素参入一个可排序的List, 并将其至于首个大于他的元素之前，如果一个List是排序的那么insert完之后仍是排序的
- genericLength genericTake genericDrop genericSplictAt genericIndex genericReplicate 不再是int类型，而是Interal


----------


## Data.Char
- isControl 判断一个字符是否是控制字符
- isSpace 判断一个字符是否为空格字符，包括空格，tab，换行符等。
- isLower 判断一个字符是否为小写
- isUper 判断一个字符是否为大写
- isAlpha 判断一个字符是否为字符
- isAlphaNum 判断一个字符是否为字符或数字
- isPrint 判断一个字符是否为可打印的
- isDigit 判断一个字符是否为数字
- isOctDigit 判断一个字符是否为八进制数字
- isHexDigit 判断一个字符是否为十六进制数字
- isLetter 判断一个字符是否为字母
- isMark 判断是否为unicode注音字符
- isNumber 判断一个字符是否为数字
- isPunctuation 判断一个字符是否为标点符号
- isSymbol 判断一个字符是否为标点符号
- isSeperater 判断一个字符是否为unicode空格或分隔符
- isAscii 判断一个字符是否在unicode字母表的前128位
- isLatinl 判断一个字符是否在unicode的字母表的前256位
- isAsciiUpper 判断一个字符是否为大写的ascii字符
- isAsciiLower 判断一个字符是否为小写的Ascii字符
- generalCategory 判断该字符的类型
- toUpper 将一个字符转为大写字符
- toLower 将一个字符转为小写字符
- toTitle 将一个字符转为title-case， 对于大多数字符而言，title-case就是大写。
- digitToInt 将一个字符转为数字，在'1'..'9', 'a'..'f', 'A'..'F'的范围之内
- intToDigit是上面函数的反函数，它取一个0-15的Int值做参数，并返回一个小写字符
- ord 与 char函数可将字符与对应的数字相互变换

-------------

## Data.Map

- Data.Map.fromList 取一个关联列表， 返回与之等价的Map。fromList :: (Ord k) => [(k, v)] -> Map k v
- Data.Map.empty 返回一个空Map
- Data.Map.insert 取一个键， 一个值， 一个Map， 将该键值对插入到该map中
- Data.Map.null 检查一个map是否为空
- Data.Map.size 返回一个map的大小
- Data.Map.singleton 取一个键值对做参数，并返回只含有一个映射的Map
- Data.Map.lookup 找到相应的键值对就返回Just something, 否则就返回Nothing
- Data.Map.member 取一个键与map做参数，并返回该键是否存在于该Map
- Data.Map.filter 和List的版本很像， 判断的是value
- Data.Map.toList 是fromList的反函数
- Data.Map.fromListWith 取一个函数和一个List, 在出现重复是调用该函数
- Data.Map.insertWith 取一个函数和List，再插入重复时调用该函数。