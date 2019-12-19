module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList  RNil        = []
rlistToList (RCons xs x) = x : rlistToList xs

listToRList :: [a] -> ReverseList a
listToRList = foldl RCons RNil

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show (RCons RNil v) = show v
    show (RCons pr v) = show pr ++ "," ++ show v
    
instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) (RCons lpr lv) (RCons rpr rv) | lv /= rv = False
                                       | otherwise  = lpr == rpr
    (==) _ _ = False
    
instance (Ord a) => Ord (ReverseList a) where
    compare RNil RNil = EQ
    compare RNil _ = LT
    compare _ RNil = GT
    compare (RCons lpr lv) (RCons rpr rv) | lv < rv = LT
                                          | lv > rv = GT
                                          | lv == rv = compare lpr rpr
                                          
instance Semigroup (ReverseList a) where
    (<>) rlst RNil = rlst
    (<>) rlst (RCons prev v) = RCons (rlst <> prev) v

instance Monoid (ReverseList a) where
  mempty = RNil
  mappend = (<>)                                        
                                          
instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons pr v) = RCons (fmap f pr) (f v)
