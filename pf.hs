{-# LANGUAGE GADTs #-}

data KULang where
    Num :: Int -> KULang 
    Boolean :: Bool -> KULang
    Id :: String -> KULang
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang  
    Mult :: KULang -> KULang -> KULang  
    Div :: KULang -> KULang -> KULang  
    Exp :: KULang -> KULang -> KULang 
    Between :: KULang -> KULang -> KULang -> KULang 
    Lambda :: String -> KULangType -> KULang -> KULang
    App :: KULang -> KULang -> KULang   
    Bind :: String -> KULang -> KULang -> KULang
    If :: KULang -> KULang -> KULang -> KULang
    And :: KULang -> KULang -> KULang
    Or :: KULang -> KULang -> KULang
    Leq :: KULang -> KULang -> KULang
    IsZero :: KULang -> KULang
    Fix :: KULang -> KULang
    Geq :: KULang -> KULang -> KULang
    Less :: KULang -> KULang -> KULang
    Greater :: KULang -> KULang -> KULang
    Eq :: KULang -> KULang -> KULang
    NotEq :: KULang -> KULang -> KULang
    Not :: KULang -> KULang
    XOr :: KULang -> KULang -> KULang
    NAnd :: KULang -> KULang -> KULang
    NOr :: KULang -> KULang -> KULang
    XNOr :: KULang -> KULang -> KULang
    Impl :: KULang -> KULang -> KULang
    Rem :: KULang -> KULang -> KULang
    Inc :: KULang -> KULang
    Dec :: KULang -> KULang
    IsEven :: KULang -> KULang
    IsOdd :: KULang -> KULang
    PythTrip :: KULang -> KULang -> KULang -> KULang 
    Gcd :: KULang -> KULang -> KULang
    Lcm :: KULang -> KULang -> KULang
    Seq :: KULang -> KULang -> KULang
    Unit :: KULang
    deriving (Show,Eq)

data KULangType where
    TNum :: KULangType 
    TBool :: KULangType 
    (:->:) :: KULangType -> KULangType -> KULangType
    TUnit :: KULangType
    deriving (Show,Eq)

data KULangVal where
    NumV :: Int -> KULangVal
    BooleanV :: Bool -> KULangVal
    ClosureV :: String -> KULang -> Env -> KULangVal
    UnitV :: KULangVal
    deriving (Show,Eq)

type Cont = [(String,KULangType)]
type Env = [(String,KULangVal)]

-- Part 1: Type Inference
typeofMonad :: Cont -> KULang -> (Maybe KULangType) 
typeofMonad c (Num x) = if x < 0 then Nothing else return TNum
typeofMonad c (Boolean x) = return TBool
typeofMonad c (Id x) = (lookup x c)
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
typeofMonad c (Between x y z) = do {TNum <- (typeofMonad c x);
                                    TNum <- (typeofMonad c y);
                                    TNum <- (typeofMonad c z);
                                    return TBool}
typeofMonad c (Lambda x d z) = do {r <- (typeofMonad ((x,d):c) z);
                                   return (d :->: r)}
typeofMonad c (App a b) = do {b' <- (typeofMonad c b);
                              (d :->: r) <- (typeofMonad c a);
                              if (b' == d) then return r else Nothing}
typeofMonad c (Bind x y z) = do {y' <- (typeofMonad c y);
                                 (typeofMonad ((x,y'):c) z)}
typeofMonad c (If x y z) = do {TBool <- (typeofMonad c x);
                               y' <- (typeofMonad c y);
                               z' <- (typeofMonad c z);
                               if (y' == z') then return y' else Nothing}
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
typeofMonad c (Fix x) = do {(d :->: r) <- (typeofMonad c x);
                            return r}
typeofMonad c (Geq x y) = do {TNum <- (typeofMonad c x);
                              TNum <- (typeofMonad c y);
                              return TBool}
typeofMonad c (Less x y) = do {TNum <- (typeofMonad c x);
                               TNum <- (typeofMonad c y);
                               return TBool}
typeofMonad c (Greater x y) = do {TNum <- (typeofMonad c x);
                                  TNum <- (typeofMonad c y);
                                  return TBool}
typeofMonad c (Eq x y) = do {TNum <- (typeofMonad c x);
                             TNum <- (typeofMonad c y);
                             return TBool}
typeofMonad c (NotEq x y) = do {TNum <- (typeofMonad c x);
                                TNum <- (typeofMonad c y);
                                return TBool}
typeofMonad c (Not x) = do {TBool <- (typeofMonad c x);
                            return TBool}
typeofMonad c (XOr x y) = do {TBool <- (typeofMonad c x);
                              TBool <- (typeofMonad c y);
                              return TBool}
typeofMonad c (NAnd x y) = do {TBool <- (typeofMonad c x);
                               TBool <- (typeofMonad c y);
                               return TBool}
typeofMonad c (NOr x y) = do {TBool <- (typeofMonad c x);
                              TBool <- (typeofMonad c y);
                              return TBool}
typeofMonad c (XNOr x y) = do {TBool <- (typeofMonad c x);
                               TBool <- (typeofMonad c y);
                               return TBool}
typeofMonad c (Impl x y) = do {TBool <- (typeofMonad c x);
                               TBool <- (typeofMonad c y);
                               return TBool}
typeofMonad c (Rem x y) = do {TNum <- (typeofMonad c x);
                              TNum <- (typeofMonad c y);
                              return TNum}
typeofMonad c (Inc x) = do {TNum <- (typeofMonad c x);
                            return TNum}
typeofMonad c (Dec x) = do {TNum <- (typeofMonad c x);
                            return TNum}
typeofMonad c (IsEven x) = do {TNum <- (typeofMonad c x);
                               return TBool}
typeofMonad c (IsOdd x) = do {TNum <- (typeofMonad c x);
                              return TBool}
typeofMonad c (PythTrip x y z) = do {TNum <- (typeofMonad c x);
                                     TNum <- (typeofMonad c y);
                                     TNum <- (typeofMonad c z);
                                     return TBool}
typeofMonad c (Gcd x y) = do {TNum <- (typeofMonad c x);
                              TNum <- (typeofMonad c y);
                              return TNum}
typeofMonad c (Lcm x y) = do {TNum <- (typeofMonad c x);
                              TNum <- (typeofMonad c y);
                              return TNum}
typeofMonad c (Seq x y) = do {(typeofMonad c x);   
                              (typeofMonad c y)}
typeofMonad c (Unit) = return TUnit

-- Part 2: Evaluation, and Part 3: Fixed Point Operator
evalMonad :: Env -> KULang -> (Maybe KULangVal)
evalMonad e (Num x) = if x < 0 then Nothing else Just (NumV x)
evalMonad e (Boolean x) = Just (BooleanV x)
evalMonad e (Id x) = (lookup x e)
evalMonad e (Plus x y) = do {(NumV x') <- (evalMonad e x);
                             (NumV y') <- (evalMonad e y);
                             if (x' + y') < 0 then Nothing else return (NumV (x' + y'))}
evalMonad e (Minus x y) = do {(NumV x') <- (evalMonad e x);
                              (NumV y') <- (evalMonad e y);
                              if (x' - y') < 0 then Nothing else return (NumV (x' - y'))}
evalMonad e (Mult x y) = do {(NumV x') <- (evalMonad e x);
                             (NumV y') <- (evalMonad e y);
                             if (x' * y') < 0 then Nothing else return (NumV (x' * y'))}
evalMonad e (Div x y) = do {(NumV x') <- (evalMonad e x);
                            (NumV y') <- (evalMonad e y);
                            if (y' == 0) then Nothing else return (NumV (x' `div` y'))}
evalMonad e (Exp x y) = do {(NumV x') <- (evalMonad e x);
                            (NumV y') <- (evalMonad e y);
                            if (x' ^ y') < 0 then Nothing else return (NumV (x' ^ y'))}
evalMonad e (Between x y z) = do {(NumV x') <- (evalMonad e x);
                                  (NumV y') <- (evalMonad e y);
                                  (NumV z') <- (evalMonad e z);
                                  return (BooleanV ((x' < y') && (y' < z')))}
evalMonad e (Lambda x y z) = Just (ClosureV x z e)
evalMonad e (App a b) = do {(ClosureV x y z) <- (evalMonad e a);
                            b' <- (evalMonad e b);
                            (evalMonad ((x,b'):z) y)}
evalMonad e (Bind x y z) = do {y' <- (evalMonad e y);
                               (evalMonad ((x,y'):e) z)}
evalMonad e (If x y z) = do {(BooleanV x') <- (evalMonad e x);
                             if x' then (evalMonad e y) else (evalMonad e z)}
evalMonad e (And x y) = do {(BooleanV x') <- (evalMonad e x);
                            (BooleanV y') <- (evalMonad e y);
                            return (BooleanV (x' && y'))}
evalMonad e (Or x y) = do {(BooleanV x') <- (evalMonad e x);
                           (BooleanV y') <- (evalMonad e y);
                           return (BooleanV (x' || y'))}
evalMonad e (Leq x y) = do {(NumV x') <- (evalMonad e x);
                            (NumV y') <- (evalMonad e y);
                            return (BooleanV (x' <= y'))}
evalMonad e (IsZero x) = do {(NumV x') <- (evalMonad e x);
                             return (BooleanV (x' == 0))} 
evalMonad e (Fix f) = do {(ClosureV i b e') <- (evalMonad e f);
                          (evalMonad e' (evalSubst i (Fix (Lambda i TNum b)) b))}
                                                                           
--Part 4: New Language Feature
evalMonad e (Geq x y) = do {(NumV x') <- (evalMonad e x);
                            (NumV y') <- (evalMonad e y);
                            return (BooleanV (x' >= y'))}
evalMonad e (Less x y) = do {(NumV x') <- (evalMonad e x);
                             (NumV y') <- (evalMonad e y);
                             return (BooleanV (x' < y'))}
evalMonad e (Greater x y) = do {(NumV x') <- (evalMonad e x);
                                (NumV y') <- (evalMonad e y);
                                return (BooleanV (x' > y'))}
evalMonad e (Eq x y) = do {(NumV x') <- (evalMonad e x);
                           (NumV y') <- (evalMonad e y);
                           return (BooleanV (x' == y'))}
evalMonad e (NotEq x y) = do {(NumV x') <- (evalMonad e x);
                              (NumV y') <- (evalMonad e y);
                              return (BooleanV (x' /= y'))}
evalMonad e (Not x) = do {(BooleanV x') <- (evalMonad e x);
                          return (BooleanV (not(x')))}
evalMonad e (XOr x y) = do {(BooleanV x') <- (evalMonad e x);
                            (BooleanV y') <- (evalMonad e y);
                            return (BooleanV (x' /= y'))}  
evalMonad e (NAnd x y) = do {(BooleanV x') <- (evalMonad e x);
                             (BooleanV y') <- (evalMonad e y);
                             return (BooleanV (not(x') || not(y')))} 
evalMonad e (NOr x y) = do {(BooleanV x') <- (evalMonad e x);
                            (BooleanV y') <- (evalMonad e y);
                            return (BooleanV (not(x') && not(y')))} 
evalMonad e (XNOr x y) = do {(BooleanV x') <- (evalMonad e x);
                             (BooleanV y') <- (evalMonad e y);
                             return (BooleanV (x' == y'))}
evalMonad e (Impl x y) = do {(BooleanV x') <- (evalMonad e x);
                             (BooleanV y') <- (evalMonad e y);
                             return (BooleanV (not(x') || y'))} 
evalMonad e (Rem x y) = do {(NumV x') <- (evalMonad e x);
                            (NumV y') <- (evalMonad e y);
                            if (y' == 0) then Nothing else return (NumV (x' `mod` y'))}
evalMonad e (Inc x) = do {(NumV x') <- (evalMonad e x);
                          if (x' + 1) < 0 then Nothing else return (NumV (x' + 1))}
evalMonad e (Dec x) = do {(NumV x') <- (evalMonad e x);
                          if (x' - 1) < 0 then Nothing else return (NumV (x' - 1))}
evalMonad e (IsEven x) = do {(NumV x') <- (evalMonad e x);
                             return (BooleanV ((x' `mod` 2) == 0))}
evalMonad e (IsOdd x) = do {(NumV x') <- (evalMonad e x);
                            return (BooleanV ((x' `mod` 2) /= 0))} 
evalMonad e (PythTrip x y z) = do {(NumV x') <- (evalMonad e x);
                                   (NumV y') <- (evalMonad e y);
                                   (NumV z') <- (evalMonad e z);
                                   return (BooleanV (x'^2 + y'^2 == z'^2))}
evalMonad e (Gcd x y) = do {(NumV x') <- (evalMonad e x);
                            (NumV y') <- (evalMonad e y);
                            return (NumV (gcd x' y'))}
evalMonad e (Lcm x y) = do {(NumV x') <- (evalMonad e x);
                            (NumV y') <- (evalMonad e y);
                            return (NumV (lcm x' y'))}
evalMonad e (Seq x y) = do {(evalMonad e x);
                            (evalMonad e y)}
evalMonad e (Unit) = Just (UnitV)
  
evalSubst :: String -> KULang -> KULang -> KULang
evalSubst x y (Num z) = (Num z)
evalSubst x y (Boolean z) = (Boolean z)
evalSubst x y (Id z) = if (x == z) then y else (Id z) 
evalSubst x y (Plus a b) = (Plus (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Minus a b) = (Minus (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Mult a b) = (Mult (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Div a b) = (Div (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Exp a b) = (Exp (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Between a b c) = (Between (evalSubst x y a) (evalSubst x y b) (evalSubst x y c))
evalSubst x y (Lambda a b c) = (Lambda a b (evalSubst x y c))
evalSubst x y (App a b) = (App (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Bind a b c) = if (x == a) then (Bind a (evalSubst x y b) c) else (Bind a (evalSubst x y b) (evalSubst x y c))
evalSubst x y (If a b c) = (If (evalSubst x y a) (evalSubst x y b) (evalSubst x y c))
evalSubst x y (And a b) = (And (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Or a b) = (Or (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Leq a b) = (Leq (evalSubst x y a) (evalSubst x y b))
evalSubst x y (IsZero z) = (IsZero (evalSubst x y z))
evalSubst x y (Fix z) = (Fix (evalSubst x y z))
evalSubst x y (Geq a b) = (Geq (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Less a b) = (Less (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Greater a b) = (Greater (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Eq a b) = (Eq (evalSubst x y a) (evalSubst x y b))
evalSubst x y (NotEq a b) = (NotEq (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Not z) = (Not (evalSubst x y z))
evalSubst x y (XOr a b) = (XOr (evalSubst x y a) (evalSubst x y b))
evalSubst x y (NAnd a b) = (NAnd (evalSubst x y a) (evalSubst x y b))
evalSubst x y (NOr a b) = (NOr (evalSubst x y a) (evalSubst x y b))
evalSubst x y (XNOr a b) = (XNOr (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Impl a b) = (Impl (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Rem a b) = (Rem (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Inc z) = (Inc (evalSubst x y z))
evalSubst x y (Dec z) = (Dec (evalSubst x y z))
evalSubst x y (IsEven z) = (IsEven (evalSubst x y z))
evalSubst x y (IsOdd z) = (IsOdd (evalSubst x y z))
evalSubst x y (PythTrip a b c) = (PythTrip (evalSubst x y a) (evalSubst x y b) (evalSubst x y c))
evalSubst x y (Gcd a b) = (Gcd (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Lcm a b) = (Lcm (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Seq a b) = (Seq (evalSubst x y a) (evalSubst x y b))
evalSubst x y (Unit) = (Unit)
                                                                                                                      
-- Part 5: Interpretation
interpretMonad :: KULang -> (Maybe KULangVal)
interpretMonad x = do {typeEval <- (typeofMonad [] x);
                       if ((typeEval == TNum) || (typeEval == TBool) || (typeEval == TUnit)) then (evalMonad [] x) else Nothing}                                         
 
--TestCases                     
evalNum = do
           print (evalMonad [] (Num 5))
           print (evalMonad [] (Num (-5)))
           
evalBoolean = do
           print (evalMonad [] (Boolean True))
           print (evalMonad [] (Boolean False))
           
evalId = do
           print (evalMonad [("x", (NumV 2))] (Id "x"))
           print (evalMonad [("y", (NumV 2))] (Id "x"))
           print (evalMonad [("x", (NumV 2))] (Id "y"))

evalPlus = do
           print (evalMonad [] (Plus (Num 6) (Num 2)))
           print (evalMonad [] (Plus (Num 6) (Num (-2))))
 
evalMinus = do
           print (evalMonad [] (Minus (Num 5) (Num 2)))
           print (evalMonad [] (Minus (Num 2) (Num 5)))
           print (evalMonad [] (Minus (Num 5) (Num (-2))))
           
evalMult = do
           print (evalMonad [] (Mult (Num 5) (Num 2)))
           print (evalMonad [] (Mult (Num 5) (Num (-2))))
           
evalDiv = do
           print (evalMonad [] (Div (Num 6) (Num 2)))
           print (evalMonad [] (Div (Num 6) (Num 0)))
           print (evalMonad [] (Div (Num 6) (Num (-2))))
           
evalExp = do
           print (evalMonad [] (Exp (Num 5) (Num 2)))
           print (evalMonad [] (Exp (Num 5) (Num (-2))))
           
evalBetween = do
           print (evalMonad [] (Between (Num 5) (Num 8) (Num 10)))
           print (evalMonad [] (Between (Num 5) (Num 2) (Num 10)))
           print (evalMonad [] (Between (Num 5) (Num 8) (Num (-10))))

evalLambda = do
           print (evalMonad [] (Lambda "x" TNum (Plus (Id "x") (Num 3))))
           print (evalMonad [] (Lambda "y" TNum ((Lambda "x" TNum (Plus (Id "x") (Id "y"))))))
           
evalApp = do
           print (evalMonad [] (App (Lambda "x" TNum (Plus (Id "x") (Num 3))) (Num 9)))
           print (evalMonad [] (App (Lambda "x" TNum (Plus (Id "x") (Num 3))) (Num (-9)))) 
           print (evalMonad [] (App (Lambda "y" TNum (App (Lambda "x" TNum (Plus (Id "x") (Id "y"))) (Num 3))) (Num 2)))
           print (evalMonad [] (App (Lambda "y" TNum (App (Lambda "x" TNum (Plus (Id "x") (Id "y"))) (Num (-3)))) (Num 2)))  
 
evalBind = do
           print (evalMonad [] (Bind "x" (Num 8) (Plus (Id "x") (Num 18))))
           print (evalMonad [] (Bind "x" (Num (-8)) (Plus (Id "x") (Num 18))))
           
evalIf = do
           print (evalMonad [] (If (Boolean True) (Num 9) (Num 10)))
           print (evalMonad [] (If (Boolean False) (Num 9) (Num 10)))
           print (evalMonad [] (If (Boolean True) (Num (-9)) (Num 10)))
           
evalAnd = do
           print (evalMonad [] (And (Boolean False) (Boolean False)))
           print (evalMonad [] (And (Boolean False) (Boolean True)))
           print (evalMonad [] (And (Boolean True) (Boolean False)))
           print (evalMonad [] (And (Boolean True) (Boolean True)))
           
evalOr = do
           print (evalMonad [] (Or (Boolean False) (Boolean False)))
           print (evalMonad [] (Or (Boolean False) (Boolean True)))
           print (evalMonad [] (Or (Boolean True) (Boolean False)))
           print (evalMonad [] (Or (Boolean True) (Boolean True)))
           
evalLeq = do
           print (evalMonad [] (Leq (Num 9) (Num 11)))
           print (evalMonad [] (Leq (Num 11) (Num 9)))
           print (evalMonad [] (Leq (Num (-9)) (Num 11)))
           
evalIsZero = do
           print (evalMonad [] (IsZero (Num 0)))
           print (evalMonad [] (IsZero (Num 10)))
           print (evalMonad [] (IsZero (Num (-10))))

evalFix = do
           print (evalMonad [] (Bind "fact" (Lambda "g" (TNum :->: TNum) (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1) (Mult (Id "x") (App (Id "g") (Minus (Id "x") (Num 1))))))) (App (Fix (Id "fact")) (Num 0))))
           print (evalMonad [] (Bind "fact" (Lambda "g" (TNum :->: TNum) (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1) (Mult (Id "x") (App (Id "g") (Minus (Id "x") (Num 1))))))) (App (Fix (Id "fact")) (Num 3))))
           print (evalMonad [] (Bind "fact" (Lambda "g" (TNum :->: TNum) (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1) (Mult (Id "x") (App (Id "g") (Minus (Id "x") (Num 1))))))) (App (Fix (Id "fact")) (Num (-1)))))
           print (evalMonad [] (Bind "fib" (Fix (Lambda "g" (TNum :->: TNum) (Lambda "x" TNum (If (Leq (Id "x") (Num 1)) (Id "x") (Plus (App (Id "g") (Minus (Id "x") (Num 1))) (App (Id "g") (Minus (Id "x") (Num 2)))))))) (App (Id "fib") (Num 1))))
           print (evalMonad [] (Bind "fib" (Fix (Lambda "g" (TNum :->: TNum) (Lambda "x" TNum (If (Leq (Id "x") (Num 1)) (Id "x") (Plus (App (Id "g") (Minus (Id "x") (Num 1))) (App (Id "g") (Minus (Id "x") (Num 2)))))))) (App (Id "fib") (Num 8))))
           print (evalMonad [] (Bind "fib" (Fix (Lambda "g" (TNum :->: TNum) (Lambda "x" TNum (If (Leq (Id "x") (Num 1)) (Id "x") (Plus (App (Id "g") (Minus (Id "x") (Num 1))) (App (Id "g") (Minus (Id "x") (Num 2)))))))) (App (Id "fib") (Num (-1)))))         

evalGeq = do
           print (evalMonad [] (Geq (Num 90) (Num 11)))
           print (evalMonad [] (Geq (Num 11) (Num 90)))
           print (evalMonad [] (Geq (Num (-90)) (Num 11)))
           
evalLess = do
           print (evalMonad [] (Less (Num 9) (Num 11)))
           print (evalMonad [] (Less (Num 11) (Num 11)))
           print (evalMonad [] (Less (Num (-9)) (Num 11)))
           
evalGreater = do
           print (evalMonad [] (Greater (Num 90) (Num 11)))
           print (evalMonad [] (Greater (Num 90) (Num 90)))
           print (evalMonad [] (Greater (Num (-90)) (Num 11)))
           
evalEq = do
           print (evalMonad [] (Eq (Num 9) (Num 9)))
           print (evalMonad [] (Eq (Num 11) (Num 9)))
           print (evalMonad [] (Eq (Num (-9)) (Num 9)))

evalNotEq = do
           print (evalMonad [] (NotEq (Num 9) (Num 11)))
           print (evalMonad [] (NotEq (Num 11) (Num 11)))
           print (evalMonad [] (NotEq (Num (-9)) (Num 11)))
           
evalNot = do
           print (evalMonad [] (Not (Boolean False)))
           print (evalMonad [] (Not (Boolean True)))

evalXOr = do
           print (evalMonad [] (XOr (Boolean False) (Boolean False)))
           print (evalMonad [] (XOr (Boolean False) (Boolean True)))
           print (evalMonad [] (XOr (Boolean True) (Boolean False)))
           print (evalMonad [] (XOr (Boolean True) (Boolean True)))

evalNAnd = do
           print (evalMonad [] (NAnd (Boolean False) (Boolean False)))
           print (evalMonad [] (NAnd (Boolean False) (Boolean True)))
           print (evalMonad [] (NAnd (Boolean True) (Boolean False)))
           print (evalMonad [] (NAnd (Boolean True) (Boolean True)))
           
evalNOr = do
           print (evalMonad [] (NOr (Boolean False) (Boolean False)))
           print (evalMonad [] (NOr (Boolean False) (Boolean True)))
           print (evalMonad [] (NOr (Boolean True) (Boolean False)))
           print (evalMonad [] (NOr (Boolean True) (Boolean True)))
           
evalXNOr = do
           print (evalMonad [] (XNOr (Boolean False) (Boolean False)))
           print (evalMonad [] (XNOr (Boolean False) (Boolean True)))
           print (evalMonad [] (XNOr (Boolean True) (Boolean False)))
           print (evalMonad [] (XNOr (Boolean True) (Boolean True)))

evalImpl = do
           print (evalMonad [] (Impl (Boolean False) (Boolean False)))
           print (evalMonad [] (Impl (Boolean False) (Boolean True)))
           print (evalMonad [] (Impl (Boolean True) (Boolean False)))
           print (evalMonad [] (Impl (Boolean True) (Boolean True)))
                     
evalRem = do
           print (evalMonad [] (Rem (Num 6) (Num 2)))
           print (evalMonad [] (Rem (Num 6) (Num 0)))
           print (evalMonad [] (Rem (Num 6) (Num (-2))))
           
evalInc = do
           print (evalMonad [] (Inc (Num 2)))
           print (evalMonad [] (Inc (Num 0)))
           print (evalMonad [] (Inc (Num (-2))))

evalDec = do
           print (evalMonad [] (Dec (Num 2)))
           print (evalMonad [] (Dec (Num 0)))
           print (evalMonad [] (Dec (Num (-2))))

evalIsEven = do
           print (evalMonad [] (IsEven (Num 2)))
           print (evalMonad [] (IsEven (Num 5)))
           print (evalMonad [] (IsEven (Num (-2))))

evalIsOdd = do
           print (evalMonad [] (IsOdd (Num 5)))
           print (evalMonad [] (IsOdd (Num 2)))
           print (evalMonad [] (IsOdd (Num (-5))))

evalPythTrip = do
           print (evalMonad [] (PythTrip (Num 3) (Num 4) (Num 5)))
           print (evalMonad [] (PythTrip (Num 5) (Num 10) (Num 13)))
           print (evalMonad [] (PythTrip (Num 5) (Num 12) (Num (-13))))

evalGcd = do
           print (evalMonad [] (Gcd (Num 98) (Num 56)))
           print (evalMonad [] (Gcd (Num 20) (Num 28)))
           print (evalMonad [] (Gcd (Num (-98)) (Num 56)))

evalLcm = do
           print (evalMonad [] (Lcm (Num 98) (Num 56)))
           print (evalMonad [] (Lcm (Num 20) (Num 28)))
           print (evalMonad [] (Lcm (Num (-20)) (Num 28)))
                                                                             
evalSeq = do
           print (evalMonad [] (Seq (Plus (Num 9) (Num 2)) (Minus (Num 8) (Num 2))))
           print (evalMonad [] (Seq (Plus (Num (-9)) (Num 2)) (Minus (Num 8) (Num 2))))
           print (evalMonad [] (Seq (Plus (Num 9) (Num 2)) (Minus (Num 2) (Num 8))))
           
evalUnit = do
           print (evalMonad [] Unit)     
                                       
testCases = do
            putStrLn "---Num---"
            evalNum
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Boolean---"
            evalBoolean
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Id---"
            evalId
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Plus---"
            evalPlus
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Minus---"
            evalMinus
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Mult---"
            evalMult
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Div---"
            evalDiv
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Exp---"
            evalExp
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Between---"
            evalBetween
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Lambda---"
            evalLambda
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---App---"
            evalApp
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Bind---"
            evalBind
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---If---"
            evalIf
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---And---"
            evalAnd
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Or---"
            evalOr
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Leq---"
            evalLeq
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---IsZero---"
            evalIsZero
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Fix---"
            evalFix
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Geq---"
            evalGeq
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Less---"
            evalLess
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Greater---"
            evalGreater
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Eq---"
            evalEq
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---NotEq---"
            evalNotEq
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Not---"
            evalNot
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---XOr---"
            evalXOr
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---NAnd---"
            evalNAnd
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---NOr---"
            evalNOr
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---XNOr---"
            evalXNOr
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Impl---"
            evalImpl
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Rem---"
            evalRem
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Inc---"
            evalInc
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Dec---"
            evalDec
            putStrLn "---------"
            putStrLn " "

            putStrLn "---IsEven---"
            evalIsEven
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---IsOdd---"
            evalIsOdd
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---PythTrip---"
            evalPythTrip
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Gcd---"
            evalGcd
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Lcm---"
            evalLcm
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Seq---"
            evalSeq
            putStrLn "---------"
            putStrLn " "
            
            putStrLn "---Unit---"
            evalUnit
            putStrLn "---------"
            putStrLn " "
            
main :: IO()
main = do
        testCases                     
