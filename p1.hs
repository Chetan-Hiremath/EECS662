
{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Token 

-- Abstract Syntax Definition
data KULang where
 Num :: Int -> KULang
 Plus :: KULang -> KULang -> KULang
 Minus :: KULang -> KULang -> KULang
 Mult :: KULang -> KULang -> KULang
 Div :: KULang -> KULang -> KULang
 Exp :: KULang -> KULang -> KULang
 deriving (Show,Eq)

-- Exercise 1
evalErr :: KULang -> Int
evalErr (Num x) = if x < 0 then error "Negative value is not allowed" else x
evalErr (Plus x y) = (evalErr x) + (evalErr y)
evalErr (Minus x y) = if ((evalErr x) - (evalErr y)) < 0 then error "Negative result is not allowed" else ((evalErr x) - (evalErr y))
evalErr (Mult x y) = (evalErr x) * (evalErr y)
evalErr (Div x y) = if (evalErr y) == 0 then error "Denominator value shouldn't be 0" else ((evalErr x) `div` (evalErr y))
evalErr (Exp x y) = (evalErr x) ^ (evalErr y)

--Exercise 2
evalMaybe :: KULang -> Maybe Int 
evalMaybe (Num x) = if x < 0 then Nothing else Just x
evalMaybe (Plus x y) = case (evalMaybe x) of
                    Just x' -> case (evalMaybe y) of
                                Just y' -> Just (x' + y')  
evalMaybe (Minus x y) = case (evalMaybe x) of
                    Just x' -> case (evalMaybe y) of
                                Just y' -> if (x' - y') < 0 then Nothing else Just (x' - y')       
evalMaybe (Mult x y) = case (evalMaybe x) of
                    Just x' -> case (evalMaybe y) of
                                Just y' -> Just (x' * y')
evalMaybe (Div x y) = case (evalMaybe x) of
                    Just x' -> case (evalMaybe y) of
                                Just y' -> if y' == 0 then Nothing else Just (x' `div` y')        
evalMaybe (Exp x y) = case (evalMaybe x) of
                    Just x' -> case (evalMaybe y) of
                                Just y' -> Just (x' ^ y')

-- Exercise 3
evalMonad :: KULang -> Maybe Int 
evalMonad (Num x) = if x < 0 then Nothing else return x
evalMonad (Plus x y) = do {x' <- (evalMonad x);
                           y' <- (evalMonad y);
                           return (x' + y')}
evalMonad (Minus x y) = do {x' <- (evalMonad x);
                            y' <- (evalMonad y);
                            if (x' - y') < 0 then Nothing else return (x' - y')}
evalMonad (Mult x y) = do {x' <- (evalMonad x);
                           y' <- (evalMonad y);
                           return (x' * y')}
evalMonad (Div x y) = do {x' <- (evalMonad x);
                          y' <- (evalMonad y);
                          if y' == 0 then Nothing else return (x' `div` y')}
evalMonad (Exp x y) = do {x' <- (evalMonad x);
                          y' <- (evalMonad y);
                          return (x' ^ y')}

-- Exercise 4
interpret :: String -> Maybe Int
interpret str = evalMonad (parseKULang str)

-- KULang Parser

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedOpNames = [ "+","-","*","^","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser KULang
expr = buildExpressionParser operators term

operators = [
                [inFix "*" Mult AssocLeft, 
                inFix "/" Div AssocLeft , 
                inFix "+" Plus AssocLeft , 
                inFix "-" Minus AssocLeft, 
                inFix "^" Exp AssocLeft]
            ]
  
numExpr :: Parser KULang 
numExpr = do i <- integer lexer
             return (Num (fromInteger i))
                     

term = parens lexer expr
       <|> numExpr

-- Parser invocation
-- Call parseKULang to parse a string into the KULang data structure.

parseKULang = parseString expr

