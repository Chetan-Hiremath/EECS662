add :: Int -> (Int -> Int)
add x y = x+y

zeroto :: Int -> [Int]
zeroto n = [0..n]

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

second :: [a] -> a
second xs = head (tail xs)

swap :: (x,y) -> (y,x)
swap (x,y) = (y,x)

pair :: x -> y -> (x,y)
pair x y = (x,y)

double :: Num x => x -> x
double x = x * 2

palindrome :: Eq xs => [xs] -> Bool
palindrome xs = reverse xs == xs

twice :: (x -> x) -> x -> x
twice f x = f(f x)

safetail :: Eq a => [a] -> [a]
safetail xs = if xs == [] then [] else tail xs

safetail' :: Eq a => [a] -> [a]
safetail' xs | xs == []       = []
             | otherwise      = tail xs

safetail'' :: Eq a => [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

sumsquares :: Int -> Int
sumsquares n = sum [x^2 | x <- [0..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

count'' :: Char -> String -> Int
count'' x xs = length [x'| x' <- xs, x == x']

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

product' :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' ns

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' []     _      = []
zip' _      []     = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n-1) xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [b | b <- xs, b > x]

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

sum'' :: Num a => [a] -> a
sum'' []     = 0 
sum'' (x:xs) = x + sum'' xs

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x xs -> f x : xs) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x xs -> if p x then (x : xs) else xs) []

foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' f v []     = v
foldr'' f v (x:xs) = f x (foldr f v xs)

length'' :: [a] -> Int
length'' = foldr (\_ n -> 1+n) 0

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs]

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or [p x | x <- xs]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

type Pos = (Int,Int)
origin :: Pos
origin = (0,0)
left :: Pos -> Pos
left (x,y) = (x-1,y)

type Pair a = (a,a)
mult' :: Pair Int -> Int
mult' (m,n) = m*n
copy :: a -> Pair a
copy x = (x,x)

data Answer = Yes | No | Unknown
answers :: [Answer]
answers = [Yes,No,Unknown]
flip' :: Answer -> Answer
flip' Yes     = No
flip' No      = Yes
flip' Unknown = Unknown

data Shape = Circle Float| Rect Float Float
square :: Float -> Shape
square n = Rect n n
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

data Maybe' a = Nothing' | Just' a
safediv :: Int -> Int -> Maybe' Int
safediv _ 0 = Nothing'
safediv m n = Just' (m `div` n)
safehead :: [a] -> Maybe' a
safehead [] = Nothing'
safehead xs = Just' (head xs)

data List a = Empty | Cons a (List a)
len :: List a -> Int
len Empty = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Empty' | Node (Tree a) a (Tree a)
size :: Tree a -> Int
size Empty' = 0
size (Node lhs _ rhs) = size lhs + 1 + size rhs
flatten :: Tree a -> [a]
flatten Empty' = []
flatten (Node lhs a rhs) = flatten lhs ++ [a] ++ flatten rhs

act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                 return []
              else
                 do xs <- getLine
                    return (x:xs)

putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = do putChar x
                    putStr xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr xs
                  putChar '\n'

sequence_' :: [IO a] -> IO ()
sequence_' []     = return ()
sequence_' (x:xs) = x >> sequence_ xs

strlen' :: IO ()
strlen' = do putStr "Enter a string: "
             xs <- getLine
             putStr "The string has "
             putStr (show (length xs))
             putStrLn " characters"

putStr'' :: String -> IO ()
putStr'' xs = sequence_ [putChar x | x <- xs]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]

primes'' :: [Int]
primes'' = sieve [2..]

twin :: (Int,Int) -> Bool
twin (x,y) = y == x+2

twins :: [(Int,Int)]
twins = filter twin (zip primes'' (tail primes''))

fibs :: [Integer]
fibs = 0:1:[ x+y | (x,y) <- zip fibs (tail fibs)]

main :: IO ()
main = putStrLn "Hello World!"  

main' :: IO ()
main' = do
        putStr "Hello"
        putChar ' '
        putStr "World!"
        putChar '\n'

main'' :: IO ()
main'' = do
         let 
           x = concat ["Hello"," ", "World!\n"]
         putStr x
