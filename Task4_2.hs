module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12
  
instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

-- Тут все строго по определению и смыслу функции fmap.
-- Берем и применяем функцию к каждому элементу множества и запаковываем все это 
-- в новое множество из четырех

instance Applicative FourOf where
    pure a = FourOf a a a a
    (<*>) (FourOf fa fb fc fd) (FourOf a b c d) = FourOf (fa a) (fb b) (fc c) (fd d)

-- Тут было желание ещ енаписать вот так:
-- pure ((a , b) , (c , d)) = FourOf a b c d
-- чтобы можно было запаковывать 4 жлемента в множество, но тип функции pure
-- говорит что не судьба
-- Поэтому раз она берет на вход только один элемент то его и продублируем 4 раза
-- Что касается <*>, так как функция по определеению должна быть запакованная, то 
-- она тоже является множеством из четырех. Поэтому берем каждую из этих четырех 
-- (на практике они могут быть и одинаковыми, но продублированными 4 раза
-- и применяем к своему аргументу соответсвенно и в результате возвращаем 
-- опять множество из 4х

instance Monad FourOf where    
     (>>=) (FourOf a b c d) f = FourOf a' b' c' d'
        where
        (FourOf a' _  _  _ ) = f a
        (FourOf _  b' _  _ ) = f b
        (FourOf _  _  c' _ ) = f c
        (FourOf _  _  _  d') = f d
        
-- В случае с монадой условие соблюдается - функция возращает упакованное значение. 
-- Но нам не нужны в результате 4 множества из 4х, нужно всего одно, поэтому мы применяем 
-- функцию к каждому из элементов поочередно, получаем 4 множества, тз каждого извлекаем нужный
-- нам по порядковому номеру элемент и соирем 4 элемнта в одно множество. 
 
main = do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y }

-- В результате функция main использует do из условия и возвращает правильное значение