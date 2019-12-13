module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}
import Prelude hiding (lookup)
import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Empty
               -- | Leaf (Integer, v)
               | Node (Integer, v) (TreeMap v) (TreeMap v)
            deriving (Show, Eq, Read)
            
-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Empty _ = False
contains (Node (key, v) left right) k 
                                 | k == key = True
                                 | k < key = contains left k
                                 | otherwise = contains right k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ Empty = error "Nonexistent key"
lookup k (Node (key, v) left right)  
                                 | k == key = v
                                 | k < key = lookup k left 
                                 | otherwise = lookup k right 

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert pair Empty = Node pair Empty Empty
insert (k,v) (Node (key, value) left right)  
                                 | k == key = error "Such key is already available"
                                 | k < key = Node (key, value) (insert (k,v) left) right
                                 | otherwise = Node (key, value) left (insert (k,v) right) 

-- Удаление элемента по ключу
findMin :: TreeMap v -> (Integer, v)
findMin (Node (key, value) Empty right) = (key, value)
findMin (Node (key, value) left right) = findMin left

testament :: TreeMap v -> TreeMap v -> TreeMap v
testament left (Node (rkey, rvalue) Empty rright) = Node (rkey, rvalue) left rright
testament left (Node (rkey, rvalue) rleft rright) = 
                                    let min = findMin rleft in 
                                    Node min left (insert min rright)

remove :: Integer -> TreeMap v -> TreeMap v
remove _ Empty = error "No such key in this tree"
remote k (Node (key, value) left right)  
             | k < key = remote k left 
             | k > key = remote k right 
             | k == key = case (left, right) of
                  (Empty, Empty) -> Empty
                  (Empty, right) -> right
                  (left, Empty) -> left 
                  (left, right) -> testament left right               

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = todo

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = todo

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = todo

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo
