module Trits where

{- this module defines the behavior of Trits
 - Trits are big endian lists -}

import Control.Exception
    ( throw,
      NonTermination(NonTermination),
      ArithException(Overflow, DivideByZero) )

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

signumTrit :: Trit -> Int
signumTrit Nega = -1
signumTrit Zero = 0
signumTrit Posi = 1

-- multiplication by negative trit
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

{-
  - 0 +
- - - -
0 - 0 0
+ - 0 +
-}
andTrit :: Trit -> Trit -> Trit
andTrit Posi Posi = Posi
andTrit a b
    | a == Nega || b == Nega = Nega
    | otherwise = Zero

{-
  - 0 +
- - 0 +
0 0 0 +
+ + + +
-}
orTrit :: Trit -> Trit -> Trit
orTrit Nega Nega = Nega
orTrit a b
    | a == Posi || b == Posi = Posi
    | otherwise = Zero

{-
  - 0 +
- - 0 +
0 0 0 0
+ + 0 -
-}
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

{-
  - 0 +
- + - 0
0 - 0 +
+ 0 + -
-}
addTrit :: Trit -> Trit -> Trit
addTrit Zero b = b
addTrit a Zero = a
addTrit a b
    | a == invertTrit b = Zero
    | a == b            = invertTrit a
    | otherwise         = error "addTrit failed!"

{-
  - 0 +
- - 0 0
0 0 0 0
+ 0 0 +
-}
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

{-
  - +
- + -
0 0 0
+ - +
-}
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

isZeros :: [Trit] -> Bool
isZeros = foldr ((&&) . (== Zero)) True

-- Remove trailing Zeros from Trits
chompTrits :: [Trit] -> [Trit]
chompTrits []    = []
chompTrits (x:xs)
    | isZeros xs = [x]
    | otherwise  = x : chompTrits xs

invertTrits :: [Trit] -> [Trit]
invertTrits = map invertTrit

posCarryTrits :: [Trit] -> [Trit]
posCarryTrits [] = [Posi]
posCarryTrits (x:xs)
    | x == Posi  = Nega : posCarryTrits xs
    | otherwise  = advanceTrit x : xs

negCarryTrits :: [Trit] -> [Trit]
negCarryTrits [] = [Nega]
negCarryTrits (x:xs)
    | x == Nega  = Posi : negCarryTrits xs
    | otherwise  = reverseTrit x : xs

addTrits :: [Trit] -> [Trit] -> [Trit]
addTrits [] [] = []
addTrits [] (y:ys) = y : ys
addTrits (x:xs) [] = x : xs
addTrits (x:xs) (y:ys)
    | (== Zero) x && isZeros xs = y : ys
    | (== Zero) y && isZeros ys = x : xs
    | carryTrit x y == Posi  = addTrit x y : addTrits xs (posCarryTrits ys)
    | carryTrit x y == Nega  = addTrit x y : addTrits xs (negCarryTrits ys)
    | otherwise              = addTrit x y : addTrits xs ys

unsafeShiftRTrits :: Integer -> [Trit] -> [Trit]
unsafeShiftRTrits _ []  = []
unsafeShiftRTrits n xs
    | n > 0             = Zero : unsafeShiftRTrits (n-1) xs
    | n == 0            = xs
    | otherwise         = throw NonTermination

unsafeShiftLTrits :: Integer -> [Trit] -> [Trit]
unsafeShiftLTrits _ []  = []
unsafeShiftLTrits n xs
    | n > 0             = unsafeShiftLTrits (n-1) $ tail xs ++ [Zero]
    | n == 0            = xs
    | otherwise         = throw NonTermination

-- unsafe right shift changes length of Trits, safe version amends this by truncation
shiftRTrits :: [Trit] -> Int -> [Trit]
shiftRTrits xs n        = take origLenXS $ unsafeShiftRTrits (fromIntegral (abs n)) xs
                            where origLenXS = length xs

shiftLTrits :: [Trit] -> Int -> [Trit]
shiftLTrits xs n        = unsafeShiftLTrits (fromIntegral (abs n)) xs

shiftTrits :: [Trit] -> Int -> [Trit]
shiftTrits xs n
    | n < 0             = shiftLTrits xs n
    | n > 0             = shiftRTrits xs n
    | otherwise         = xs

multTritsByTrit :: Trit -> [Trit] -> [Trit]
multTritsByTrit Posi xs = xs
multTritsByTrit Zero xs = [Zero]
multTritsByTrit Nega xs = invertTrits xs

multTrits :: [Trit] -> [Trit] -> [Trit]
multTrits [] [] = [Zero]
multTrits xs [] = [Zero]
multTrits [] ys = [Zero]
multTrits xs ys = foldr addTrits [Zero] inlineProducts
    where inlineProducts = [unsafeShiftRTrits order value | (order, value) <- zip [0..] lnProds]
          lnProds        = [multTritsByTrit x ys | x <- xs]

exptTrits :: [Trit] -> Integer -> [Trit]
exptTrits x n
    | n > 0     = multTrits x (exptTrits x (n-1))
    | n == 0    = [Posi]
    | otherwise = throw NonTermination

trits2Int :: [Trit] -> Integer
trits2Int [] = 0
trits2Int xs = sum [d | d <- [trit2Int n x | (n, x) <- zip [0..] xs]]

-- can technically convert any positive integer, just inefficient
oneDigit2Trits :: Integer -> [Trit]
oneDigit2Trits n
    | n > 0     = posCarryTrits $ oneDigit2Trits (n-1)
    | n == 0    = []
    | otherwise = throw NonTermination

tenInTrits :: [Trit]
tenInTrits = [Posi,Zero,Posi]

digits2Trits :: Integer -> [[Trit]]
digits2Trits 0 = []
digits2Trits x = oneDigit2Trits (x `mod` 10) : digits2Trits (x `div` 10)

unsignedInt2Trits :: Integer -> [Trit]
unsignedInt2Trits x
    | x > 0     = foldr addTrits [] [multTrits order digit | (order, digit)
                            <- zip [exptTrits tenInTrits n | n <- [0..]] (digits2Trits x)]
    | x == 0    = [Zero]
    | otherwise = throw NonTermination

int2Trits :: Integer -> [Trit]
int2Trits x
    | x < 0     = invertTrits $ unsignedInt2Trits (-x)
    | x > 0     = unsignedInt2Trits x
    | otherwise = []

signumTrits :: [Trit] -> Trit
signumTrits []  = Zero
signumTrits x   = last (chompTrits x)

absTrits :: [Trit] -> [Trit]
absTrits x = multTrits [signumTrits x] x

fixLenTrits :: Int -> [Trit] -> [Trit]
fixLenTrits 0 []     = []
fixLenTrits 0 xs
    | isZeros xs     = []
    | otherwise      = throw Overflow
fixLenTrits 1 [x]    = [x]
fixLenTrits n []     = Zero : fixLenTrits (n-1) []
fixLenTrits n (x:xs) = x : fixLenTrits (n-1) xs

maxTrits :: Int -> [Trit]
maxTrits n
    | n == 0    = []
    | n > 0     = Posi : maxTrits (n-1)
    | otherwise = throw NonTermination

minTrits :: Int -> [Trit]
minTrits n
    | n == 0    = []
    | n > 0     = Nega : minTrits (n-1)
    | otherwise = throw NonTermination

trits2Str :: [Trit] -> String
trits2Str = foldr ((:) . trit2Chr) ""

-- eq class operations on trits
eqTrits, neqTrits :: [Trit] -> [Trit] -> Bool
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
xs `neqTrits` ys              = not $ xs `eqTrits` ys

leTrits, ltTrits, geTrits, gtTrits :: [Trit] -> [Trit] -> Bool
[] `ltTrits` []                 = False
[x] `ltTrits` [y]               = x < y
[] `ltTrits` [y]
    | y == Posi                 = True
    | otherwise                 = False
[x] `ltTrits` []
    | x == Nega                 = True
    | otherwise                 = False
[] `ltTrits` ys
    | signumTrits ys == Posi    = True
    | otherwise                 = False
xs `ltTrits` []
    | signumTrits xs == Nega    = True
    | otherwise                 = False
xs `ltTrits` ys
    | length (chompTrits xs)
    == length (chompTrits ys)   = (last xs < last ys)
                                    || ((last xs == last ys)
                                        && (init xs `ltTrits` init ys))
    | length (chompTrits xs)
    < length (chompTrits ys)    = (signumTrits xs < signumTrits ys)
                                    || ((signumTrits xs == signumTrits ys)
                                        && (signumTrits xs == Posi))
    | otherwise                 = (signumTrits xs > signumTrits ys)
                                    || ((signumTrits xs == signumTrits ys)
                                        && (signumTrits xs == Nega))
xs `gtTrits` ys                 = not $ xs `ltTrits` ys
xs `leTrits` ys                 = xs `eqTrits` ys || xs `ltTrits` ys
xs `geTrits` ys                 = xs `eqTrits` ys || xs `gtTrits` ys

compareTrits :: [Trit] -> [Trit] -> Ordering
xs `compareTrits` ys
    | xs `ltTrits` ys           = LT
    | xs `eqTrits` ys           = EQ
    | otherwise                 = GT

andTrits, orTrits, xorTrits :: [Trit] -> [Trit] -> [Trit]
xs `andTrits` ys                = [x `andTrit` y | (x, y) <- zip xs ys]
xs `orTrits` ys                 = [x `orTrit` y | (x, y) <- zip xs ys]
xs `xorTrits` ys                = [x `xorTrit` y | (x, y) <- zip xs ys]

rotateRTrits :: [Trit] -> Int -> [Trit]
rotateRTrits xs n
    | n > 0                     = rotateRTrits (last xs : init xs) (n-1)
    | n == 0                    = xs
    | otherwise                 = throw NonTermination

rotateLTrits :: [Trit] -> Int -> [Trit]
rotateLTrits xs n
    | n > 0                     = rotateLTrits (tail xs ++ [head xs]) (n-1)
    | n == 0                    = xs
    | otherwise                 = throw NonTermination

rotateTrits :: [Trit] -> Int -> [Trit]
rotateTrits xs n
    | n > 0                     = rotateLTrits xs n
    | n < 0                     = rotateRTrits xs n
    | otherwise                 = xs

-- definition of typeclass Trits
class Eq a => Trits a where
    (.&.)           :: a -> a -> a
    (.|.)           :: a -> a -> a
    xor             :: a -> a -> a
    complement      :: a -> a
    shift           :: a -> Int -> a
    rotate          :: a -> Int -> a
    tritSize        :: a -> Int
    testTrit        :: a -> Int -> Trit
    setTrit         :: a -> Trit -> Int -> a
    nand            :: a -> a -> a
    xs `nand` ys    = complement $ xs .&. ys
    nor             :: a -> a -> a
    xs `nor` ys     = complement $ xs .|. ys
    xnor            :: a -> a -> a
    xs `xnor` ys    = complement $ xs `xor` ys