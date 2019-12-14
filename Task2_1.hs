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
remove k (Node (key, value) left right)  
             | k < key = remove k left 
             | k > key = remove k right 
             | k == key = case (left, right) of
                  (Empty, Empty) -> Empty
                  (Empty, right) -> right
                  (left, Empty) -> left 
                  (left, right) -> testament left right               

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i Empty = error "Could not find"
nearestLE i (Node (k, v) l r) | k == i = (k, v)
                              | k > i = nearestLE i l
                              | k < i = case(r) of
                                        Node (k, v) left _  | k == i -> (k, v)
                                                            | k < i -> nearestLE i r
                                                            | k > i -> case (left) of
                                                                 Empty -> (k, v)
                                                                 otherwise -> nearestLE i left
                                        otherwise -> (k, v)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = Empty
treeFromList lst = foldr insert Empty lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Empty = []
listFromTree (Node (k, v) l r) = (k,v):listFromTree (remove k (Node (k, v) l r))


-- Поиск k-той порядковой статистики дерева
tSize :: TreeMap v -> Integer
tSize Empty = 0
tSize (Node _ left right ) = (tSize left) + 1 + (tSize right)

kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Empty = error "Tree is empty"
kMean i (Node (key, value) left right ) | tSize left == i = (key, value)
                                        | tSize left > i  = kMean i left
                                        | otherwise     = kMean (i - (tSize left) - 1) right
 