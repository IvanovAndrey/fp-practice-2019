module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show Zero = "Zero"
    show (Succ child) = concat [show child, " -> Succ"]
    show (Pred child) = concat ["Pred -> ", show child]

wpnToInteger :: WeirdPeanoNumber -> Integer
wpnToInteger Zero = 0
wpnToInteger (Succ prev) = wpnToInteger prev + 1
wpnToInteger (Pred prev) = wpnToInteger prev - 1    

wpnFromInteger :: Integer -> WeirdPeanoNumber
wpnFromInteger p | p == 0 = Zero
                 | p > 0 = Succ $ wpnFromInteger (p - 1)
                 | p < 0 = Pred $ wpnFromInteger (p + 1)

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize = wpnFromInteger . wpnToInteger
 
wpnToRational :: WeirdPeanoNumber -> Rational
wpnToRational Zero = 0
wpnToRational (Succ prev) = toRational prev + 1
wpnToRational (Pred prev) = toRational prev - 1
 
wpnToEnum :: Int -> WeirdPeanoNumber
wpnToEnum p | p == 0 = Zero
            | p > 0 = Succ $ wpnToEnum (p - 1)
            | p < 0 = Pred $ wpnToEnum (p + 1)

wpnFromEnum :: WeirdPeanoNumber -> Int
wpnFromEnum Zero = 0
wpnFromEnum (Succ prev) = wpnFromEnum prev + 1
wpnFromEnum (Pred prev) = wpnFromEnum prev - 1 
 
add :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
add num Zero = num
add num (Succ prev) = Succ $ add prev num
add num (Pred prev) = Pred $ add prev num

negation :: WeirdPeanoNumber -> WeirdPeanoNumber
negation Zero = Zero
negation (Succ prev) = Pred $ negation prev
negation (Pred prev) = Succ $ negation prev 

sign :: WeirdPeanoNumber -> WeirdPeanoNumber
sign Zero = Zero
sign (Succ child) = Succ Zero
sign (Pred child) = Pred Zero   
 
wpnQuot a b = if (a - b >= Zero)
                then (quot (a - b)  b) + 1 
                else Zero
                
wpnRem a b = if (a - b >= Zero)
                then wpnRem (a - b) b
                else a

instance Eq WeirdPeanoNumber where
    (==) p1 p2 = wpnToInteger p1 == wpnToInteger p2
    
instance Ord WeirdPeanoNumber where
    compare p1 p2
        | wpnToInteger p1 > wpnToInteger p2 = GT
        | wpnToInteger p1 < wpnToInteger p2 = LT
        | wpnToInteger p1 == wpnToInteger p2 = EQ   
        
instance Num WeirdPeanoNumber where
    (+) p1 p2 = add p1 p2
    
    negate  =  negation 
    
    signum p = sign (normalize p)
    
    abs p = if signum p >= Zero then p else negate p
    
    fromInteger   = wpnFromInteger    
    
    (*) p1 p2 = case signum p2 of
        Succ Zero -> p1 + p1 * Pred p2
        Pred Zero -> negate $ p1 * abs p2
        Zero -> Zero
        
instance Enum WeirdPeanoNumber where
  fromEnum = wpnFromEnum
  toEnum   = wpnToEnum 

instance Real WeirdPeanoNumber where
  toRational = wpnToRational

instance Integral WeirdPeanoNumber where
    toInteger Zero = 0
    toInteger (Succ a) = toInteger a + 1
    toInteger (Pred a) = toInteger a - 1
    quotRem a b = let signab = signum a * signum b
                      absA = abs a
                      absB = abs b
                      in if(b == Zero) then error "Can't divide zero"
                                       else
                                       (signab *(wpnQuot absA absB), signab * (wpnRem absA absB))