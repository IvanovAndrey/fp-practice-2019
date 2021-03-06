module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так
instance Semigroup (PSet a) where
    (PSet f1) <> (PSet f2) = PSet (\x -> (f1 x) && (f2 x))
 
instance Monoid (PSet a) where
    mempty = PSet (\x -> True)
      
-- По определению моноида: "Функция mappend используется для комбинирования пар элементов,
-- а mempty представляет собой нейтральный элемент." 
-- В данном случае комбинировать можно тремя способами - с помощью логического И, ИЛИ, а так же с
-- помощью XOR. 
-- Так как функция вида mappend (PSet f1) (PSet f2) (или в моем случае (PSet f1) <> (PSet f2), 
-- так как мне компилятор не дает написать без Semigroup, видимо это связано с версией языка)
-- может определяться в таком виде только один раз, для реалиации всех трех видов обьединения 
-- придется заводить еще два newtype PSet, обьединение которых работало бы на других двух логических опрациях.
-- Так как Haskell язык ленивый и этим отдаленно похож на пишущего на нем меня - 
-- было принято решение реализовать только один вариант, на основе логического И

instance Functor PSet where
    fmap f (PSet fa) = PSet (\fb -> False)
    
 -- Нормальный функтор на заданной функции реализовать не получается в виду недостаточности начального условия. 
 -- В рамках языка Haskell ешил реализовать такой, который всегда возвращает False.
 -- Он хотя бы работает.