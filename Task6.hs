module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции

Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf ['0'..'9']

intNum :: Parser String
intNum = many1 digit

floatPart :: Parser String
floatPart = do
                char '.'
                res <- many1 digit
                return ('.':res)

number :: Parser Float
number = do
            num <- intNum
            opt <- option "" floatPart
            return $ read (num ++ opt)
            
negative :: Parser Float
negative = atom 
              <|> do
                   char '-'
                   spaces
                   res <- atom
                   return $ negate res
                   
fac :: Float -> Float
fac 1 = 1
fac n | n - toEnum(truncate(n)) == 0 = n  * fac (n - 1)
      | otherwise = error "Not-integer factorial"
                                        
applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t


div_ :: Parser (Float -> Float -> Float)
div_ = do
    char '/'
    return (/)

star :: Parser (Float -> Float -> Float)
star = do
    char '*'
    return (*)

plus :: Parser (Float -> Float -> Float)
plus = do
    char '+'
    return (+)

minus :: Parser (Float -> Float -> Float)
minus = do
    char '-'
    return (-)
    
factorial :: Parser (Float -> Float)
factorial = do
    char '!'
    return fac
    
factoriation :: Parser Float
factoriation = do
    spaces
    x <- negative
    spaces
    ys <- many (factorial)
    return $ foldl (\ x f -> f x) x ys                
   
multiplication :: Parser Float
multiplication = do
    spaces
    lhv <- factoriation
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- star <|> div_
                spaces
                rhv <- factoriation
                spaces
                return (`f` rhv)

addition :: Parser Float
addition = do
    spaces
    lhv <- multiplication
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- plus <|> minus
                spaces
                rhv <- multiplication
                spaces
                return (`f` rhv)

atom :: Parser Float
atom = number <|> do
    char '('
    res <- addition
    char ')'
    return res

mainParser :: Parser Float
mainParser = do
            spaces
            p <- addition
            eof
            return p