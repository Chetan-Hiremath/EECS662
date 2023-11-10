{-# LANGUAGE GADTs, FlexibleContexts #-}


-- AST Definition
data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang 
    deriving (Show,Eq)

data KULang where
    Num :: Int -> KULang 
    Boolean :: Bool -> KULang
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang  
    Mult :: KULang -> KULang -> KULang  
    Div :: KULang -> KULang -> KULang   
    Exp :: KULang -> KULang -> KULang 
    And :: KULang -> KULang -> KULang   
    Or :: KULang -> KULang -> KULang  
    Leq :: KULang -> KULang -> KULang  
    IsZero :: KULang -> KULang  
    If :: KULang -> KULang -> KULang -> KULang  
    Between :: KULang -> KULang -> KULang -> KULang
    deriving (Show,Eq)

-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Type Inference

-- Exercise 1
evalMonad :: KULang -> Maybe KULang
evalMonad (Num x) = if x < 0 then Nothing else Just (Num x)
evalMonad (Boolean x) = Just (Boolean x)
evalMonad (Plus x y) = do {(Num x') <- (evalMonad x);
                           (Num y') <- (evalMonad y);
                           if (x' + y') < 0 then Nothing else return (Num (x' + y'))}
evalMonad (Minus x y) = do {(Num x') <- (evalMonad x);
                            (Num y') <- (evalMonad y);
                            if (x' - y') < 0 then Nothing else return (Num (x' - y'))}
evalMonad (Mult x y) = do {(Num x') <- (evalMonad x);
                           (Num y') <- (evalMonad y);
                           if (x' * y') < 0 then Nothing else return (Num (x' * y'))}
evalMonad (Div x y) = do {(Num x') <- (evalMonad x);
                          (Num y') <- (evalMonad y);
                          if (y' == 0) then Nothing else return (Num (x' `div` y'))}
evalMonad (Exp x y) = do {(Num x') <- (evalMonad x);
                          (Num y') <- (evalMonad y);
                          if (x' ^ y') < 0 then Nothing else return (Num (x' ^ y'))}
evalMonad (And x y) = do {(Boolean x') <- (evalMonad x);
                          (Boolean y') <- (evalMonad y);
                          return (Boolean (x' && y'))}
evalMonad (Or x y) = do {(Boolean x') <- (evalMonad x);
                         (Boolean y') <- (evalMonad y);
                         return (Boolean (x' || y'))}
evalMonad (Leq x y) = do {(Num x') <- (evalMonad x);
                          (Num y') <- (evalMonad y);
                          return (Boolean (x' <= y'))}
evalMonad (IsZero x) = do {(Num x') <- (evalMonad x);
                           return (Boolean (x' == 0))}
evalMonad (If x y z) = do {(Boolean x') <- (evalMonad x);
                           if x' then (evalMonad y) else (evalMonad z)}
evalMonad (Between x y z) = do {(Num x') <- (evalMonad x);
                                (Num y') <- (evalMonad y);
                                (Num z') <- (evalMonad z);
                                return (Boolean ((x' < y') && (y' < z')))}


-- Exercise 2
typeofMonad :: KULang -> Maybe KUTypeLang
typeofMonad (Num x) = if x >= 0 then return TNum else Nothing
typeofMonad (Boolean x) = return TBool 
typeofMonad (Plus x y) = do {TNum <- (typeofMonad x);
                             TNum <- (typeofMonad y);
                             return TNum}
typeofMonad (Minus x y) = do {(Num x') <- (evalMonad x);
                              (Num y') <- (evalMonad y);
                              if (x' < y') then Nothing else return TNum}
typeofMonad (Mult x y) = do {TNum <- (typeofMonad x);
                             TNum <- (typeofMonad y);
                             return TNum}
typeofMonad (Div x y) = do {TNum <- (typeofMonad x);
                            (Num y') <- (evalMonad y);
                            if (y' == 0) then Nothing else return TNum}
typeofMonad (Exp x y) = do {TNum <- (typeofMonad x);
                            TNum <- (typeofMonad y);
                            return TNum}   
typeofMonad (And x y) = do {TBool <- (typeofMonad x);
                            TBool <- (typeofMonad y); 
                            return TBool}
typeofMonad (Or x y) = do {TBool <- (typeofMonad x);
                           TBool <- (typeofMonad y); 
                           return TBool}
typeofMonad (Leq x y) = do {(Num x') <- (evalMonad x);
                            (Num y') <- (evalMonad y); 
                            return TBool}
typeofMonad (IsZero x) = do {(Num x') <- (evalMonad x);
                             return TBool}  
typeofMonad (If x y z) = do {TBool <- (typeofMonad x);
                             y' <- (typeofMonad y);
                             z' <- (typeofMonad z);
                             if (y' == z') then return y' else Nothing}
typeofMonad (Between x y z) = do {(Num x') <- (evalMonad x);
                                  (Num y') <- (evalMonad y);
                                  (Num z') <- (evalMonad z);
                                  return TBool}                

-- Exercise 3
interpTypeEval :: KULang -> Maybe KULang
interpTypeEval eval = do {typeEval <- (typeofMonad eval);
                          evalMonad eval}

-- Part 2: Optimizer

-- Exercise 1
optimize :: KULang -> KULang
optimize (Num x) = (Num x)
optimize (Boolean x) = (Boolean x)
optimize (Plus x (Num 0)) = (optimize x)
optimize (Plus (Num 0) y) = (optimize y)
optimize (Plus x y) = (Plus (optimize x) (optimize y))
optimize (Minus x (Num 0)) = (optimize x)
optimize (Minus x y) = (Minus (optimize x) (optimize y))
optimize (Mult x (Num 0)) = (Num 0)
optimize (Mult (Num 0) y) = (Num 0)
optimize (Mult x (Num 1)) = (optimize x)
optimize (Mult (Num 1) y) = (optimize y)
optimize (Mult x y) = (Mult (optimize x) (optimize y))
optimize (Div (Num 0) y) = (Num 0)
optimize (Div x (Num 1)) = (optimize x)
optimize (Div x y) = (Div (optimize x) (optimize y))
optimize (Exp (Num 0) y) = (Num 0)
optimize (Exp x (Num 0)) = (Num 1)
optimize (Exp (Num 1) y) = (Num 1)
optimize (Exp x (Num 1)) = (optimize x)
optimize (Exp x y) = (Exp (optimize x) (optimize y))
optimize (And (Boolean True) y) = (optimize y)
optimize (And (Boolean False) y) = (Boolean False)
optimize (Or (Boolean True) y) = (Boolean True)
optimize (Or (Boolean False) y) = (optimize y)
optimize (Leq (Num 0) y) = (Boolean True)
optimize (Leq x (Num 0)) = (Boolean False)
optimize (Leq x y) = (Leq (optimize x) (optimize y))
optimize (IsZero (Num 0)) = (Boolean True)
optimize (IsZero x) = (Boolean False)
optimize (If (Boolean True) y z) = (optimize y)
optimize (If (Boolean False) y z) = (optimize z)
optimize (Between x y z) = (Between (optimize x) (optimize y) (optimize z))

-- Exercise 2
interpOptEval :: KULang -> Maybe KULang
interpOptEval eval = evalMonad (optimize eval)
