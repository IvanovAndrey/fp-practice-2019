module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index DNil _ = error "Empty list"
index (DCons _ v r) 0 = v
index (DCons _ v r) i = index r (i - 1)

insertAt :: DList a -> Int -> a -> DList a
insertAt DNil 0 val = DCons DNil val DNil
insertAt (DCons l v (DCons _ rv rr)) 0 val = rec where
    rec = DCons l val newr
    newr = DCons rec v (DCons newr rv rr)
insertAt (DCons l v DNil) 0 val = rec where
    rec = DCons l val r
    r = DCons rec v DNil
insertAt (DCons l v r) index val = DCons l v $ insertAt r (index - 1) val
insertAt DNil _ _ = error "Incorrect index"

removeAt :: DList a -> Int -> DList a
removeAt DNil _ = error "Empty list"
removeAt (DCons _ _ DNil) 0 = DNil
removeAt (DCons _ _ DNil) _ = error "Incorrect index"
removeAt (DCons l _ (DCons _ rv rr)) 0 = DCons l rv rr
removeAt (DCons l val r) index = DCons l val $ removeAt r (index - 1)
 
b = DNil
a = list2dlist [1,2,3,4,5]
