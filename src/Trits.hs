module Trits where

{- this module defines the behavior of Trits
 - Trits are big endian lists -}

import Control.Exception

data Trit = Nega | Zero | Posi deriving Show

instance Eq Trit where
    Nega == Nega = True
    Zero == Zero = True
    Posi == Posi = True
    _ == _ = False

instance Ord Trit where
    Nega <= Zero = True
    Zero <= Posi = True
    Nega <= Posi = True
    Nega <= Nega = True
    Zero <= Zero = True
    Posi <= Posi = True
    _ <= _ = False

isNega :: Trit -> Bool
isNega Nega = True
isNega _    = False

isZero :: Trit -> Bool
isZero Zero = True
isZero _    = False

isPosi :: Trit -> Bool
isPosi Posi = True
isPosi _    = False

signumTrit :: Trit -> Int
signumTrit Nega = -1
signumTrit Zero = 0
signumTrit Posi = 1

invertTrit :: Trit -> Trit
invertTrit Nega = Posi
invertTrit Posi = Nega
invertTrit Zero = Zero

-- cause a trit to rotate through values positively
advanceTrit :: Trit -> Trit
advanceTrit Nega = Zero
advanceTrit Zero = Posi
advanceTrit Posi = Nega

-- cause a trit to rotate through values negatively
reverseTrit :: Trit -> Trit
reverseTrit Posi = Zero
reverseTrit Zero = Nega
reverseTrit Nega = Posi

andTrit :: Trit -> Trit -> Trit
andTrit Posi Posi = Posi
andTrit a b
    | a == Nega || b == Nega = Nega
    | otherwise = Zero

orTrit :: Trit -> Trit -> Trit
orTrit Nega Nega = Nega
orTrit a b
    | a == Posi || b == Posi = Posi
    | otherwise = Zero

xorTrit :: Trit -> Trit -> Trit
xorTrit Posi Posi = Nega
xorTrit Nega Nega = Nega
xorTrit a b
    | a == invertTrit b = Posi
    | otherwise = Zero

nandTrit :: Trit -> Trit -> Trit
nandTrit a b = invertTrit (andTrit a b)

norTrit :: Trit -> Trit -> Trit
norTrit a b = invertTrit (orTrit a b)

xnorTrit :: Trit -> Trit -> Trit
xnorTrit a b = invertTrit (xorTrit a b)

addTrit :: Trit -> Trit -> Trit
addTrit Zero b = b
addTrit a Zero = a
addTrit a b
    | a == invertTrit b = Zero
    | a == b            = invertTrit a
    | otherwise         = error "addTrit failed!"

carryTrit :: Trit -> Trit -> Trit
carryTrit a b
    | a == b    = a
    | otherwise = Zero

{-}
addAndCarryTrit :: Trit -> Trit -> (Trit, Trit)
addAndCarryTrit a b = (addTrit a b, carryTrit a b)
-}

--multTrit is just an alias for xnorTrit
multTrit :: Trit -> Trit -> Trit
multTrit = xnorTrit

divTrit :: Trit -> Trit -> Trit
divTrit _ Zero = throw DivideByZero
divTrit x y
    | x == invertTrit y = Nega
    | x == y            = Posi
    | otherwise         = Zero

-- integer argument is the order of magnitude of the second trit argument
trit2Int :: Integer -> Trit -> Integer
trit2Int x Nega = -1 * 3^x
trit2Int x Zero = 0 * 3^x
trit2Int x Posi = 1 * 3^x

trit2Chr :: Trit -> Char
trit2Chr Nega = '-'
trit2Chr Zero = '0'
trit2Chr Posi = '+'

{- "Trits" definitions
 - These definitions codify the behavior of lists of Trits
 - Lists of Trits are big endian (makes the recursion easier)
 -}

type Trits = [Trit]

isZeros :: Trits -> Bool
isZeros = foldr ((&&) . isZero) True

-- Remove trailing Zeros from Trits
chompTrits :: Trits -> Trits
chompTrits []    = []
chompTrits (x:xs)
    | isZeros xs = [x]
    | otherwise  = x : chompTrits xs

invertTrits :: Trits -> Trits
invertTrits = map invertTrit

posCarryTrits :: Trits -> Trits
posCarryTrits [] = [Posi]
posCarryTrits (x:xs)
    | x == Posi  = Nega : posCarryTrits xs
    | otherwise  = advanceTrit x : xs

negCarryTrits :: Trits -> Trits
negCarryTrits [] = [Nega]
negCarryTrits (x:xs)
    | x == Nega  = Posi : negCarryTrits xs
    | otherwise  = reverseTrit x : xs

addTrits :: Trits -> Trits -> Trits
addTrits [] [] = []
addTrits [] (y:ys) = y : ys
addTrits (x:xs) [] = x : xs
addTrits (x:xs) (y:ys)
    | isZero x && isZeros xs = y : ys
    | isZero y && isZeros ys = x : xs
    | carryTrit x y == Posi  = addTrit x y : addTrits xs (posCarryTrits ys)
    | carryTrit x y == Nega  = addTrit x y : addTrits xs (negCarryTrits ys)
    | otherwise              = addTrit x y : addTrits xs ys

shiftTrits :: Integer -> Trits -> Trits
shiftTrits _ [] = []
shiftTrits n xs
    | n > 0     = Zero : shiftTrits (n-1) xs
    | n == 0    = xs
    | otherwise = throw NonTermination

multTritsByTrit :: Trit -> Trits -> Trits
multTritsByTrit Posi xs = xs
multTritsByTrit Zero xs = [Zero]
multTritsByTrit Nega xs = invertTrits xs

multTrits :: Trits -> Trits -> Trits
multTrits [] [] = [Zero]
multTrits xs [] = [Zero]
multTrits [] ys = [Zero]
multTrits xs ys = foldr addTrits [Zero] inlineProducts
    where inlineProducts = [shiftTrits order value | (order, value) <- zip [0..] lnProds]
          lnProds        = [multTritsByTrit x ys | x <- xs]

exptTrits :: Trits -> Integer -> Trits
exptTrits x n
    | n > 0     = multTrits x (exptTrits x (n-1))
    | n == 0    = [Posi]
    | otherwise = throw NonTermination

trits2Int :: Trits -> Integer
trits2Int [] = 0
trits2Int xs = sum [d | d <- [trit2Int n x | (n, x) <- zip [0..] xs]]

-- can technically convert any positive integer, just inefficient
oneDigit2Trits :: Integer -> Trits
oneDigit2Trits n
    | n > 0     = posCarryTrits $ oneDigit2Trits (n-1)
    | n == 0    = []
    | otherwise = throw NonTermination

tenInTrits :: Trits
tenInTrits = [Posi,Zero,Posi]

digits2Trits :: Integer -> [Trits]
digits2Trits 0 = []
digits2Trits x = oneDigit2Trits (x `mod` 10) : digits2Trits (x `div` 10)

unsignedInt2Trits :: Integer -> Trits
unsignedInt2Trits x
    | x > 0     = foldr addTrits [] [multTrits order digit | (order, digit)
                            <- zip [exptTrits tenInTrits n | n <- [0..]] (digits2Trits x)]
    | x == 0    = [Zero]
    | otherwise = throw NonTermination

int2Trits :: Integer -> Trits
int2Trits x
    | x < 0     = invertTrits $ unsignedInt2Trits (-x)
    | x > 0     = unsignedInt2Trits x
    | otherwise = []

signumTrits :: Trits -> Trit
signumTrits x = last (chompTrits x)

fixLenTrits :: Int -> Trits -> Trits
fixLenTrits 0 xs     = throw Overflow
fixLenTrits 1 [x]    = [x]
fixLenTrits n []     = Zero : fixLenTrits (n-1) []
fixLenTrits n (x:xs) = x : fixLenTrits (n-1) xs

trits2Str :: Trits -> String
trits2Str = foldr ((:) . trit2Chr) ""

-- eq class operations on trits
eqTrits, neqTrits :: Trits -> Trits -> Bool
[] `eqTrits` []               = True
[] `eqTrits` [y]
    | y == Zero               = True
    | otherwise               = False
[x] `eqTrits` []
    | x == Zero               = True
    | otherwise               = False
[x] `eqTrits` [y]             = x == y
[] `eqTrits` (y:ys)
    | y == Zero && isZeros ys = True
    | otherwise               = False
(x:xs) `eqTrits` []
    | x == Zero && isZeros xs = True
    | otherwise               = False
(x:xs) `eqTrits` (y:ys)       = x == y && xs `eqTrits` ys

neqTrits                      = not . eqTrits

ltTrits, leTrits, gtTrits, geTrits :: Trits -> Trits -> Bool
[] `ltTrits` []               = False
