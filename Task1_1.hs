module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Operator = Plus | Minus | Multiply deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, op::Operator, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l Plus r

(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l Minus r

(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l Multiply r

infixl 1 |+|
infixl 1 |-|
infixl 2 |*|

--Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
    case expression of
        Variable v -> if(v == varName) then replacement else expression
        BinaryTerm lhv op rhv -> BinaryTerm (replaceVar varName  replacement lhv) op (replaceVar varName replacement rhv) 
        _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = 
    case expression of
        BinaryTerm lhv op rhv -> 
            case (evaluate(lhv), op, evaluate(rhv)) of 
                (IntConstant lhv, Plus, IntConstant rhv) -> IntConstant (lhv + rhv)
                (IntConstant lhv, Minus, IntConstant rhv) -> IntConstant (lhv - rhv)
                (IntConstant lhv, Multiply, IntConstant rhv) -> IntConstant (lhv * rhv)
                _ -> BinaryTerm lhv op rhv
        _-> expression













