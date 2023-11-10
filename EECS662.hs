{-# LANGUAGE GADTs #-}

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  And :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  deriving (Show,Eq)

eval :: ABE -> Maybe ABE
eval (Num x) = Just (Num x)
eval (Boolean x) = Just (Boolean x)
eval (Plus t1 t2) = do { (Num v1) <- (eval t1);
                         (Num v2) <- (eval t2);
                         return (Num (v1 + v2)) }
eval (Minus t1 t2) = do { (Num v1) <- (eval t1);
                          (Num v2) <- (eval t2);
                          return (Num (v1 - v2)) }
eval (And t1 t2) = do { (Boolean v1) <- (eval t1);
                        (Boolean v2) <- (eval t2);
                        return (Boolean (v1 && v2)) }                        
eval (Leq t1 t2) = do { (Num v1) <- (eval t1);
                        (Num v2) <- (eval t2);
                        return (Boolean (v1 <= v2)) } 
                                           
data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)
  
typeof :: ABE -> Maybe TABE
typeof (Num n) = if n>=0 then return TNum else Nothing
typeof (Boolean b) = return TBool
typeof (Plus l r) = do {TNum <- typeof l;
                        TNum <- typeof r;
                        return TNum}
typeof (And l r) = do {TBool <- typeof l;
                       TBool <- typeof r;
                       return TBool}
typeof (IsZero t) = do {TNum <- typeof t;
                       return TBool}
