module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (h:t) = foldl f (f z h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (h:t) = f h (foldr f z t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f a = case f a of
               Just (h, next) -> h:(unfoldr f next)
               Nothing      -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f lst = foldr (mapFunc f) [] lst
    
mapFunc :: (a -> b) -> a -> [b]-> [b]
mapFunc f h t =  ((f h) : t)
-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldl (*) 1 

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\i lst -> case i of
                                 Just i -> i:lst
                                 Nothing -> lst) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = todo


-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot = todo

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e [] = False
elem e lst = foldl (||) False (map (\x -> x == e) lst) 

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\x -> if x < to then Just (x, x+step) else Nothing) from
                                        

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append first second = foldr (\x res -> x:res)  second first

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = todo