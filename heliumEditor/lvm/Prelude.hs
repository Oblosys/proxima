{- The Standard Prelude for the Helium Compiler -}

module Prelude(module Prelude, module PreludePrim) where 

import PreludePrim

infixr 8  ^ 



(^) :: Num a => a -> Int -> a
_ ^ 0           = fromInt 1
i ^ n  | n > 0  = f i (n-1) i
          where f _ 0 y = y
                f x m y = g x m
                          where g x' m' | even m'    = g (x' * x') (m' `quot` 2)
                                        | otherwise  = f x' (m' - 1) (x' * y)
_ ^ _           = error "Prelude.^: negative exponent"



even :: Int -> Bool
even n = n `rem` 2 == 0

otherwise :: Bool
otherwise = True

undefined :: a
undefined = case True of { False -> undefined }


-- duplicated:

length :: [a] -> Int
length xs = foldl' (\l _ -> l + 1) 0 xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ a [] = a
foldl' f a (x:xs) = (foldl' f $! f a x) xs

--

{- 
For Proxima, we don't need much from the prelude yet. Until compiling is done in a more incremental
way, we use a stripped prelude.

infixr 9  .
infixl 9  !!
infixr 8  ^ -- , **.
-- infixl 7  *, `quot`, `rem`, `div`, `mod`,  /                [PreludePrim]
-- infixl 6  +, -                                              [PreludePrim]
infixr 5  ++
-- infixr 5 :                                                  [HeliumLang]
-- infix  4  ==, /=, <=, <, >, >=                              [PreludePrim]
infixr 3  &&
infixr 2  ||
infixr 0  $ --, $!                                             [PreludePrim]

{-----------------------------------------------
 -- Num
 -----------------------------------------------}

{- imported from PreludePrim

(+)     :: Num a => a -> a -> a
(-)     :: Num a => a -> a -> a
(*)     :: Num a => a -> a -> a
negate  :: Num a => a -> a
fromInt :: Num a => Int -> a
-}

sum :: Num a => [a] -> a
sum = foldl' (+) (fromInt 0)

product :: Num a => [a] -> a
product = foldl' (*) (fromInt 1)

{-----------------------------------------------
 -- Eq
 -----------------------------------------------}

{- imported from PreludePrim

(==) :: Eq a => a -> a -> Bool
(/=) :: Eq a => a -> a -> Bool
-}

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) 
  | x == y = True
  | otherwise = elem x ys
  
notElem :: Eq a => a -> [a] -> Bool
notElem x ys = not (x `elem` ys)

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup _ []       = Nothing
lookup k ((x,y):xys)
      | k == x  = Just y
      | otherwise = lookup k xys  

{-----------------------------------------------
 -- Ord
 -----------------------------------------------}

{- imported from PreludePrim

(<)     :: Ord a => a -> a -> Bool
(<=)    :: Ord a => a -> a -> Bool
(>)     :: Ord a => a -> a -> Bool
(>=)    :: Ord a => a -> a -> Bool
compare :: Ord a => a -> a -> Ordering
-}

max :: Ord a => a -> a -> a
max x y = if x < y then y else x

min :: Ord a => a -> a -> a
min x y = if x < y then x else y

maximum :: Ord a => [a] -> a
maximum = foldl1 max

minimum :: Ord a => [a] -> a
minimum = foldl1 min

{-----------------------------------------------
 -- Int
 -----------------------------------------------}

{- imported from PreludePrim
rem  :: Int -> Int -> Int
div  :: Int -> Int -> Int
mod  :: Int -> Int -> Int
quot :: Int -> Int -> Int
-}

abs :: Int -> Int
abs x = if x < 0 then - x else x

signum :: Int -> Int
signum x =
    case compare x 0 of
        LT -> -1
        EQ ->  0
        GT ->  1

even :: Int -> Bool
even n = n `rem` 2 == 0

odd :: Int -> Bool
odd  n = not (even n)

subtract :: Int -> Int -> Int
subtract a b = b - a

gcd :: Int -> Int -> Int
gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y = gcd' (abs x) (abs y)
   where gcd' :: Int -> Int -> Int
         gcd' x' 0  = x'
         gcd' x' y' = gcd' y' (x' `rem` y')

lcm :: Int -> Int -> Int
lcm _ 0 = 0
lcm 0 _ = 0
lcm x y = abs ((x `quot` gcd x y) * y)

(^) :: Num a => a -> Int -> a
_ ^ 0           = fromInt 1
i ^ n  | n > 0  = f i (n-1) i
          where f _ 0 y = y
                f x m y = g x m
                          where g x' m' | even m'    = g (x' * x') (m' `quot` 2)
                                        | otherwise  = f x' (m' - 1) (x' * y)
_ ^ _           = error "Prelude.^: negative exponent"

{-----------------------------------------------
 -- Float
 -----------------------------------------------}

{- imported from PreludePrim

(/)   :: Float -> Float -> Float
sqrt  :: Float -> Float
(**.) :: Float -> Float -> Float
exp   :: Float -> Float
log   :: Float -> Float
sin   :: Float -> Float
cos   :: Float -> Float
tan   :: Float -> Float
-}

absFloat :: Float -> Float
absFloat x = if x < 0.0 then -. x else x

signumFloat :: Float -> Int
signumFloat x =
    case compare x 0.0 of
        LT -> -1
        EQ ->  0
        GT ->  1

{-
(^.) :: Float -> Int -> Float
_ ^. 0           = 1.0
i ^. n  | n > 0  = f i (n-1) i
          where f :: Float -> Int -> Float -> Float
                f _ 0 y = y
                f x m y = g x m
                          where g :: Float -> Int -> Float
                                g x' m' | even m'    = g (x' * x') (m' `quot` 2)
                                        | otherwise  = f x' (m' - 1) (x' * y)
_ ^. _           = error "Prelude.^.: negative exponent"
-}

pi :: Float
pi = 3.141592653589793

{-----------------------------------------------
 -- Bool
 -----------------------------------------------}

not :: Bool -> Bool
not False = True
not _ = False

(||) :: Bool -> Bool -> Bool
(&&) :: Bool -> Bool -> Bool

x || y = if x then x else y
x && y = if x then y else x

otherwise :: Bool
otherwise = True

{-----------------------------------------------
 -- Maybe
 -----------------------------------------------}

data Maybe a
    = Nothing
    | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe e f m =
    case m of 
        Nothing -> e
        Just x  -> f x

{-----------------------------------------------
 -- Either
 -----------------------------------------------}

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either l r e =
    case e of 
        Left  x -> l x
        Right y -> r y

{-----------------------------------------------
 -- Ordering
 -----------------------------------------------}

{- imported from PreludePrim

data Ordering = LT | EQ | GT 
-}

{-----------------------------------------------
 -- Tuple
 -----------------------------------------------}

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, x) = x

curry          :: ((a,b) -> c) -> (a -> b -> c)
curry f x y     = f (x,y)

uncurry        :: (a -> b -> c) -> ((a,b) -> c)
uncurry f p     = f (fst p) (snd p)

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith  (\a b -> (a,b))

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 = zipWith3 (\a b c -> (a,b,c))

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith _ _      _        = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a:as) (b:bs) (c:cs)
              = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _          = []

unzip :: [(a,b)] -> ([a],[b])
unzip = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([], [])

unzip3:: [(a,b,c)] -> ([a],[b],[c])
unzip3 = foldr (\(a,b,c) (as,bs,cs) -> (a:as,b:bs,c:cs)) ([],[],[])

{-----------------------------------------------
 -- List
 -----------------------------------------------}

head :: [a] -> a
head (x:_) = x
head _ = error "Prelude.head: empty list"

last :: [a] -> a
last [x] = x
last (_:xs) = last xs
last _ = error "Prelude.last: empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail _ = error "Prelude.tail: empty list"

init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs
init _ = error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null _  = False

(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x : (xs ++ ys)
[]     ++ ys = ys

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs) 
    | p x =  x : filter p xs 
    | otherwise = filter p xs
filter _ [] = []

{- 
Naive implementation of length (slow because of laziness)

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

Optimised implementation using strict foldl:
-}

length :: [a] -> Int
length xs = foldl' (\l _ -> l + 1) 0 xs

concat :: [[a]] -> [a]
concat = foldr (++) []

(!!) :: [a] -> Int -> a
_      !! n | n < 0 = error "Prelude.(!!): negative index"
(x:_)  !! 0         = x
(_:xs) !! n         = xs !! (n - 1)
[]     !! _         = error "Prelude.(!!): index too large"

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ a [] = a
foldl' f a (x:xs) = (foldl' f $! f a x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xxs =
    q
    :
    (case xxs of 
        x:xs -> scanl f (f q x) xs
        []   -> []
    )

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f xxs =
    case xxs of 
        x:xs -> scanl f x xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = x `f` (foldr f z xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 xxs =
    case xxs of 
        [] ->
            [q0]
        x:xs -> 
            let qs = scanr f q0 xs in
                case qs of { q:_ -> f x q : qs }

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f xxs =
    case xxs of 
        x:xs ->
            case xs of 
                [] ->
                    [x]
                _ ->
                    let qs = scanr1 f xs in
                        case qs of { q:_ -> f x q : qs }

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x:xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs' where xs'=xs++xs'

take :: Int -> [a] -> [a]
take n _  | n <= 0  = []
take _ []           = []
take n (x:xs)       = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0  = xs
drop _ []           = []
drop n (_:xs)       = drop (n-1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs | n <= 0 = ([],xs)
splitAt _ []          = ([],[])
splitAt n (x:xs)      = (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
    | p x = x : takeWhile p xs 
    | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p l@(x:xs)
    | p x = dropWhile p xs 
    | otherwise = l

span :: (a -> Bool) -> [a] -> ([a],[a])
span _ []            = ([],[])
span p xs@(x:xs')
     | p x       = (x:ys, zs)
     | otherwise = ([],xs)
                       where (ys,zs) = span p xs'

break :: (a -> Bool) -> [a] -> ([a],[a])
break p = span (not . p)

lines :: String -> [String]
lines ""   = []
lines s    = let l,s' :: String
                 (l,s') = break (\x -> x == '\n') s
             in l : case s' of []      -> []
                               (_:s'') -> lines s''

words :: String -> [String]
words s =
    case dropWhile isSpace s of
        "" -> []
        s' -> w : words s''
              where w,s'' :: String
                    (w,s'') = break isSpace s'

unlines :: [String] -> String
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls

unwords :: [String] -> String
unwords [] = ""
unwords [w] = w
unwords (w:ws) = w ++ ' ' : unwords ws

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

{-----------------------------------------------
 -- Char
 -----------------------------------------------}

isSpace :: Char -> Bool
isSpace c =
    let
        i :: Int
        i = ord c
    in
        i == ord ' '  ||
        i == ord '\t' ||
        i == ord '\n' ||
        i == ord '\r' ||
        i == ord '\f' ||
        i == ord '\v'
        
isUpper :: Char -> Bool
isUpper c = ord c >= ord 'A' && ord c <= ord 'Z' 

isLower :: Char -> Bool
isLower c = ord c >= ord 'a' && ord c <= ord 'z'

isDigit :: Char -> Bool
isDigit c = ord c >= ord '0' && ord c <= ord '9'

isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

isAlphaNum :: Char -> Bool
isAlphaNum c =  isAlpha c || isDigit c

toUpper :: Char -> Char
toUpper c
    | isLower c = chr ( ord c - ord 'a' + ord 'A' )
    | otherwise = c

toLower :: Char -> Char
toLower c
    | isUpper c = chr ( ord c - ord 'A' + ord 'a' )
    | otherwise = c

{-----------------------------------------------
 -- Conversion
 -----------------------------------------------}

-- see also "read.." and "show.." below

{- imported from PreludePrim

ord :: Char -> Int
chr :: Int -> Char

intToFloat :: Int -> Float
round      :: Float -> Int
floor      :: Float -> Int
ceiling    :: Float -> Int
truncate   :: Float -> Int
-}

{-----------------------------------------------
 -- Some standard functions
 -----------------------------------------------}

fix :: (a -> a) -> a
fix f = x where x = f x 

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
f $ x = f x

{- imported from PreludePrim

seq :: a -> b -> b
($!) :: (a -> b) -> a -> b
error :: String -> a

-}

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

undefined :: a
undefined = case True of { False -> undefined }

{-----------------------------------------------
 -- IO
 -----------------------------------------------}

(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) = primBindIO

{- imported from PreludePrim 

return :: a -> IO a
-}

sequence :: [IO a] -> IO [a]
sequence [] = return []
sequence (c:cs) = do { x <- c; xs <- sequence cs; return (x:xs) }

sequence_ :: [IO a] -> IO ()
sequence_ [] = return ()
sequence_ (c:cs) = do { c; sequence_ cs; return () }

putChar :: Char -> IO ()
putChar c = primPutChar c

putStr :: String -> IO ()
putStr s = primPutStr s 

putStrLn :: String -> IO ()
putStrLn s = primPutStrLn s

unsafePerformIO :: IO a -> a 
unsafePerformIO = primUnsafePerformIO

print :: (a -> String) -> a -> IO ()
print showElement e = putStrLn (showElement e)

getLine   :: IO String
getLine = do 
    c <- getChar
    if c == '\n' 
        then return ""
        else do cs <- getLine
                return (c:cs)

{-----------------------------------------------
 -- Read
 -----------------------------------------------}

readInt :: String -> Int
readInt [] = 0
readInt ('-':s) = - readUnsigned s
readInt s = readUnsigned s

readUnsigned :: String -> Int
readUnsigned = 
    foldl (\a b -> a * 10 + b) 0
    .
    map (\c -> ord c - ord '0')
    .
    takeWhile isDigit
-}