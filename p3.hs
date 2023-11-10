{-# LANGUAGE GADTs,FlexibleContexts #-}


-- AST and Type Definitions

data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang
    deriving (Show,Eq)

data KULang where
    Num :: Int -> KULang  
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang 
    Div :: KULang -> KULang -> KULang  
    Exp :: KULang -> KULang -> KULang
    Boolean :: Bool -> KULang  
    And :: KULang -> KULang -> KULang 
    Or :: KULang -> KULang -> KULang  
    Leq :: KULang -> KULang -> KULang  
    IsZero :: KULang -> KULang  
    If :: KULang -> KULang -> KULang -> KULang 
    Between :: KULang -> KULang -> KULang -> KULang
    Bind :: String -> KULang -> KULang -> KULang
    Id :: String -> KULang
    deriving (Show,Eq)


type Env = [(String,KULang)]

type Cont = [(String,KUTypeLang)]


-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Adding Booleans

-- Exercise 1
evalDirect :: KULang -> (Maybe KULang)
evalDirect (Num x) = if x < 0 then Nothing else Just (Num x)
evalDirect (Plus x y) = do {(Num x') <- (evalDirect x);
                            (Num y') <- (evalDirect y);
                            if (x' + y') < 0 then Nothing else return (Num (x' + y'))}
evalDirect (Minus x y) = do {(Num x') <- (evalDirect x);
                             (Num y') <- (evalDirect y);
                             if (x' - y') < 0 then Nothing else return (Num (x' - y'))}
evalDirect (Mult x y) = do {(Num x') <- (evalDirect x);
                            (Num y') <- (evalDirect y);
                            if (x' * y') < 0 then Nothing else return (Num (x' * y'))}
evalDirect (Div x y) = do {(Num x') <- (evalDirect x);
                           (Num y') <- (evalDirect y);
                           if (y' == 0) then Nothing else return (Num (x' `div` y'))}
evalDirect (Exp x y) = do {(Num x') <- (evalDirect x);
                           (Num y') <- (evalDirect y);
                           if (x' ^ y') < 0 then Nothing else return (Num (x' ^ y'))}
evalDirect (Boolean x) = Just (Boolean x)
evalDirect (And x y) = do {(Boolean x') <- (evalDirect x);
                           (Boolean y') <- (evalDirect y);
                           return (Boolean (x' && y'))}
evalDirect (Or x y) = do {(Boolean x') <- (evalDirect x);
                          (Boolean y') <- (evalDirect y);
                          return (Boolean (x' || y'))}
evalDirect (Leq x y) = do {(Num x') <- (evalDirect x);
                           (Num y') <- (evalDirect y);
                           return (Boolean (x' <= y'))}
evalDirect (IsZero x) = do {(Num x') <- (evalDirect x);
                            return (Boolean (x' == 0))}
evalDirect (If x y z) = do {(Boolean x') <- (evalDirect x);
                           if x' then (evalDirect y) else (evalDirect z)}
evalDirect (Between x y z) = do {(Num x') <- (evalDirect x);
                                 (Num y') <- (evalDirect y);
                                 (Num z') <- (evalDirect z);
                                 return (Boolean ((x' < y') && (y' < z')))}
evalDirect (Bind x y z) = do {y' <- (evalDirect y);
                              evalDirect (evalSubst x y' z)}
evalDirect (Id x) = Nothing

evalSubst :: String -> KULang -> KULang -> KULang
evalSubst x y (Num z) = (Num z)
evalSubst x y (Plus a b) = (Plus (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Minus a b) = (Minus (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Mult a b) = (Mult (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Div a b) = (Div (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Exp a b) = (Exp (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Boolean z) = (Boolean z)
evalSubst x y (And a b) = (And (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Or a b) = (Or (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Leq a b) = (Leq (evalSubst x y a) (evalSubst x y b))
evalSubst x y (IsZero z) = (IsZero (evalSubst x y z))
evalSubst x y (If a b c) = (If (evalSubst x y a) (evalSubst x y b) (evalSubst x y c))
evalSubst x y (Between a b c) = (Between (evalSubst x y a) (evalSubst x y b) (evalSubst x y c))
evalSubst x y (Bind a b c) = if (x == a) then (Bind a (evalSubst x y b) c) else (Bind a (evalSubst x y b) (evalSubst x y c))
evalSubst x y (Id z) = if (x == z) then y else (Id z) 

-- Exercise 2
evalDeferred :: Env -> KULang -> (Maybe KULang)
evalDeferred e (Num x) = if x < 0 then Nothing else Just (Num x)
evalDeferred e (Plus x y) = do {(Num x') <- (evalDeferred e x);
                                (Num y') <- (evalDeferred e y);
                                if (x' + y') < 0 then Nothing else return (Num (x' + y'))}
evalDeferred e (Minus x y) = do {(Num x') <- (evalDeferred e x);
                                 (Num y') <- (evalDeferred e y);
                                 if (x' - y') < 0 then Nothing else return (Num (x' - y'))}
evalDeferred e (Mult x y) = do {(Num x') <- (evalDeferred e x);
                                (Num y') <- (evalDeferred e y);
                                if (x' * y') < 0 then Nothing else return (Num (x' * y'))}
evalDeferred e (Div x y) = do {(Num x') <- (evalDeferred e x);
                               (Num y') <- (evalDeferred e y);
                               if (y' == 0) then Nothing else return (Num (x' `div` y'))}
evalDeferred e (Exp x y) = do {(Num x') <- (evalDeferred e x);
                               (Num y') <- (evalDeferred e y);
                               if (x' ^ y') < 0 then Nothing else return (Num (x' ^ y'))}
evalDeferred e (Boolean x) = Just (Boolean x)
evalDeferred e (And x y) = do {(Boolean x') <- (evalDeferred e x);
                               (Boolean y') <- (evalDeferred e y);
                               return (Boolean (x' && y'))}
evalDeferred e (Or x y) = do {(Boolean x') <- (evalDeferred e x);
                              (Boolean y') <- (evalDeferred e y);
                              return (Boolean (x' || y'))}
evalDeferred e (Leq x y) = do {(Num x') <- (evalDeferred e x);
                               (Num y') <- (evalDeferred e y);
                               return (Boolean (x' <= y'))}
evalDeferred e (IsZero x) = do {(Num x') <- (evalDeferred e x);
                                return (Boolean (x' == 0))}
evalDeferred e (If x y z) = do {(Boolean x') <- (evalDeferred e x);
                                if x' then (evalDeferred e y) else (evalDeferred e z)}
evalDeferred e (Between x y z) = do {(Num x') <- (evalDeferred e x);
                                     (Num y') <- (evalDeferred e y);
                                     (Num z') <- (evalDeferred e z);
                                     return (Boolean ((x' < y') && (y' < z')))}
evalDeferred e (Bind x y z) = do {y' <- (evalDeferred e y);
                                  (evalDeferred ((x,y'):e) z)}
evalDeferred e (Id x) = (lookup x e)


-- Exercise 3
testEvals :: KULang -> Bool
testEvals x = if ((evalDeferred [] x) == (evalDirect x)) then True else False

-- Part 2: Type Checking

--Exercise 1
typeofMonad :: Cont -> KULang -> (Maybe KUTypeLang)
typeofMonad c (Num x) = if x < 0 then Nothing else Just TNum
typeofMonad c (Plus x y) = do {TNum <- (typeofMonad c x);
                               TNum <- (typeofMonad c y);
                               return TNum}
typeofMonad c (Minus x y) = do {TNum <- (typeofMonad c x);
                                TNum <- (typeofMonad c y);
                                return TNum}
typeofMonad c (Mult x y) = do {TNum <- (typeofMonad c x);
                               TNum <- (typeofMonad c y);
                               return TNum}
typeofMonad c (Div x y) = do {TNum <- (typeofMonad c x);
                              TNum <- (typeofMonad c y);
                              return TNum}
typeofMonad c (Exp x y) = do {TNum <- (typeofMonad c x);
                              TNum <- (typeofMonad c y);
                              return TNum}
typeofMonad c (Boolean x) = Just TBool
typeofMonad c (And x y) = do {TBool <- (typeofMonad c x);
                              TBool <- (typeofMonad c y);
                              return TBool}
typeofMonad c (Or x y) = do {TBool <- (typeofMonad c x);
                             TBool <- (typeofMonad c y);
                             return TBool}
typeofMonad c (Leq x y) = do {TNum <- (typeofMonad c x);
                              TNum <- (typeofMonad c y);
                              return TBool}
typeofMonad c (IsZero x) = do {TNum <- (typeofMonad c x);
                              return TBool}
typeofMonad c (If x y z) = do {TBool <- (typeofMonad c x);
                               y' <- (typeofMonad c y);
                               z' <- (typeofMonad c z);
                               if (y' == z') then return y' else Nothing}
typeofMonad c (Between x y z) = do {TNum <- (typeofMonad c x);
                                    TNum <- (typeofMonad c y);
                                    TNum <- (typeofMonad c z);
                                    return TBool}
typeofMonad c (Bind x y z) = do {y' <- (typeofMonad c y);
                                 (typeofMonad ((x,y'):c) z)}
typeofMonad c (Id x) = (lookup x c)

--Exercise 2
interpret :: KULang -> (Maybe KULang)
interpret eval = do {typeofMonad [] eval;
                     evalDeferred [] eval}
