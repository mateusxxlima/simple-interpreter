module Interpreter where

import Parser 

-- Types definitions
---------------------
data Ty = TBool
        | TNum
        | TPair Ty Ty
        deriving Show

-- Function that evaluates an execution step
--------------------------------------------
step :: Expr -> Maybe Expr
step (Mult (Num n1) (Num n2)) = Just (Num (n1 * n2))
step (Mult (Num n) e2) = 
    case (step e2) of
        Just e2' -> Just (Mult (Num n) e2')
        Nothing  -> Nothing                        
step (Mult e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (Mult e1' e2)
        Nothing  -> Nothing
-- S-Add
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
-- S-Add2 
step (Add (Num n) e2) = 
    case (step e2) of
        Just e2' -> Just (Add (Num n) e2')
        Nothing  -> Nothing                        
-- S-Add1
step (Add e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (Add e1' e2)
        Nothing  -> Nothing
-- S-And2
step (And BTrue e2) = Just e2
-- S-And3
step (And BFalse e2) = Just BFalse
-- S-And1
step (And e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (And e1' e2)
        Nothing  -> Nothing
step (Or BTrue e2) = Just BTrue
step (Or BFalse e2) = Just e2
step (Or e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (Or e1' e2)
        Nothing  -> Nothing
step (If BTrue e1 e2) = Just e1
step (If BFalse e1 e2) = Just e2
step (If e1 e2 e3) = case (step e1) of
                       Just e1' -> Just (If e1' e2 e3)
                       Nothing  -> Nothing
step (Pair e1 e2) = case (step e1, step e2) of
                        (Just e1', Just e2') -> Just (Pair e1' e2')
                        _                    -> Nothing
step (First (Pair e1 e2)) = Just e1
step (Second (Pair e1 e2)) = Just e2
step e = Just e

-- Function that evaluates an expression until it returns a
-- result or throw an error
----------------------------------------------------
eval :: Expr -> Maybe Expr
eval e = case (step e) of 
           Just e' -> if (e == e') then
                        Just e
                      else
                        eval e'
           _ -> error "Semantic error: erro avaliando expressão!" 

-- Function that checks the type of an expression
----------------------------------------------
typeof :: Expr -> Maybe Ty 
typeof BTrue = Just TBool
typeof BFalse = Just TBool
typeof (Num _) = Just TNum
typeof (Add e1 e2) = case (typeof e1) of
                       Just TNum -> case (typeof e2) of
                                      Just TNum -> Just TNum
                                      _         -> Nothing -- type error
                       _         -> Nothing -- type error
typeof (Mult e1 e2) = case (typeof e1) of
                       Just TNum -> case (typeof e2) of
                                      Just TNum -> Just TNum
                                      _         -> Nothing -- type error
                       _         -> Nothing -- type error
typeof (And e1 e2) = case (typeof e1, typeof e2) of 
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing -- type error
typeof (Or e1 e2) = case (typeof e1, typeof e2) of 
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing -- type error
typeof (If e1 e2 e3) = case (typeof e1) of 
                         Just TBool -> case (typeof e2, typeof e3) of 
                                         (Just TBool, Just TBool) -> Just TBool
                                         (Just TNum, Just TNum)   -> Just TNum
                                         _                        -> Nothing -- type error
                         _          -> Nothing -- type error

typeof (Pair e1 e2) = case (typeof e1, typeof e2) of
                        (Just t1, Just t2) -> Just (TPair t1 t2)
                        _                  -> Nothing

typeof (First e) = case (typeof e) of
                        Just (TPair e1 e2) -> Just (TPair e1 e2)
                        _                  -> Nothing

typeof (Second e) = case (typeof e) of
                        Just (TPair e1 e2) -> Just (TPair e1 e2)
                        _                  -> Nothing

-- Function that checks the type of an expression
----------------------------------------
typecheck :: Expr -> Expr
typecheck e = case (typeof e) of 
                Just _ -> e
                _      -> error "Type error: erro na verificação de tipos!"


------------------------------------------
-- Read the codes and call the interpreter
------------------------------------------
main = getContents >>= print . eval . typecheck . parser . lexer 

