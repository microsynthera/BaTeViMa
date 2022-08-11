module Trits where

{- this module defines the Trits typeclass
for balanced ternary arithmetic and tritwise operations on
variable width lists of trits -}

import Control.Exception
    ( throw,
      NonTermination(NonTermination),
      ArithException(Overflow, DivideByZero) )

import Data.Ix ( Ix )

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

-- produces the decimal value associated with a single trit in the one's place
signumTrit :: Trit -> Int
signumTrit Nega = -1
signumTrit Zero = 0
signumTrit Posi = 1

-- multiplication by negative trit, logical inversion
invertTrit :: Trit -> Trit
invertTrit Nega = Posi
invertTrit Posi = Nega
invertTrit Zero = Zero

-- cause a trit to rotate through values positively = [..., -, 0, +, -, ...]
rollUpTrit :: Trit -> Trit
rollUpTrit Nega = Zero
rollUpTrit Zero = Posi
rollUpTrit Posi = Nega

-- cause a trit to rotate through values negatively = [..., +, 0, -, +, ...]
rollDownTrit :: Trit -> Trit
rollDownTrit Posi = Zero
rollDownTrit Zero = Nega
rollDownTrit Nega = Posi

{-
  - 0 +
- - - -
0 - 0 0
+ - 0 +
-}
andTrits :: Trit -> Trit -> Trit
andTrits Posi Posi = Posi
andTrits a b
    | a == Nega || b == Nega = Nega
    | otherwise = Zero

{-
  - 0 +
- - 0 +
0 0 0 +
+ + + +
-}
orTrits :: Trit -> Trit -> Trit
orTrits Nega Nega = Nega
orTrits a b
    | a == Posi || b == Posi = Posi
    | otherwise = Zero

{-
  - 0 +
- - 0 +
0 0 0 0
+ + 0 -
-}
xorTrits :: Trit -> Trit -> Trit
xorTrits Posi Posi = Nega
xorTrits Nega Nega = Nega
xorTrits a b
    | a == invertTrit b = Posi
    | otherwise = Zero

nandTrits :: Trit -> Trit -> Trit
nandTrits a b = invertTrit (andTrits a b)

norTrits :: Trit -> Trit -> Trit
norTrits a b = invertTrit (orTrits a b)

xnorTrits :: Trit -> Trit -> Trit
xnorTrits a b = invertTrit (xorTrits a b)

{-
  - 0 +
- + - 0
0 - 0 +
+ 0 + -
-}
addTrits :: Trit -> Trit -> Trit
addTrits Zero b = b
addTrits a Zero = a
addTrits a b
    | a == invertTrit b = Zero
    | a == b            = invertTrit a
    | otherwise         = error "addTrits failed!"

{-
  - 0 +
- - 0 0
0 0 0 0
+ 0 0 +
-}
carryTrits :: Trit -> Trit -> Trit
carryTrits a b
    | a == b    = a
    | otherwise = Zero

{-
addAndCarryTrit :: Trit -> Trit -> (Trit, Trit)
addAndCarryTrit a b = (addTrits a b, carryTrits a b)
-}
{-
--multTrits is just an alias for xnorTrits
multTrits :: Trit -> Trit -> Trit
multTrits = xnorTrits
-}
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
-- result is the decimal integer value of the balaced ternary digit
trit2Int :: Integer -> Trit -> Integer
trit2Int x Nega = -1 * 3^x
trit2Int x Zero = 0 * 3^x
trit2Int x Posi = 1 * 3^x
{-
trit2Chr :: Trit -> Char
trit2Chr Nega = '-'
trit2Chr Zero = '0'
trit2Chr Posi = '+'
-}
{- "LOT" (List Of Trits) definitions
 - These definitions codify the behavior of lists of Trits (LOT)
 - Lists of Trits are big endian (makes the recursion easier)
 -}

isZeroes :: [Trit] -> Bool
isZeroes = foldr ((&&) . (== Zero)) True

-- Remove trailing Zeros from Trits
chompLOT, invertLOT, posCarryLOT, negCarryLOT :: [Trit] -> [Trit]
chompLOT []         = []
chompLOT (x:xs)
    | isZeroes xs   = [x]
    | otherwise     = x : chompLOT xs

invertLOT           = map invertTrit

posCarryLOT []      = [Posi]
posCarryLOT (x:xs)
    | x == Posi     = Nega : posCarryLOT xs
    | otherwise     = rollUpTrit x : xs

negCarryLOT []      = [Nega]
negCarryLOT (x:xs)
    | x == Nega     = Posi : negCarryLOT xs
    | otherwise     = rollDownTrit x : xs

addLOT :: [Trit] -> [Trit] -> [Trit]
addLOT [] []                        = []
addLOT [] (y:ys)                    = y : ys
addLOT (x:xs) []                    = x : xs
addLOT (x:xs) (y:ys)
    | (== Zero) x && isZeroes xs    = y : ys
    | (== Zero) y && isZeroes ys    = x : xs
    | carryTrits x y == Posi        = addTrits x y : addLOT xs (posCarryLOT ys)
    | carryTrits x y == Nega        = addTrits x y : addLOT xs (negCarryLOT ys)
    | otherwise                     = addTrits x y : addLOT xs ys

unsafeShiftRLOT, unsafeShiftLLOT :: Integer -> [Trit] -> [Trit]
unsafeShiftRLOT _ []    = []
unsafeShiftRLOT n xs
    | n > 0             = Zero : unsafeShiftRLOT (n-1) xs
    | n == 0            = xs
    | otherwise         = throw NonTermination

unsafeShiftLLOT _ []    = []
unsafeShiftLLOT n xs
    | n > 0             = unsafeShiftLLOT (n-1) $ tail xs ++ [Zero]
    | n == 0            = xs
    | otherwise         = throw NonTermination

-- unsafe right shift changes length of Trits, safe version amends this by truncation
shiftRLOT, shiftLLOT, shiftLOT, unsafeShiftLOT :: [Trit] -> Int -> [Trit]
shiftRLOT xs n        = take origLenXS $ unsafeShiftRLOT (fromIntegral (abs n)) xs
                            where origLenXS = length xs

shiftLLOT xs n        = unsafeShiftLLOT (fromIntegral (abs n)) xs

shiftLOT xs n
    | n < 0             = shiftLLOT xs n
    | n > 0             = shiftRLOT xs n
    | otherwise         = xs

unsafeShiftLOT xs n
    | n > 0             = unsafeShiftLLOT (fromIntegral n) xs
    | n < 0             = unsafeShiftRLOT (fromIntegral n) xs
    | otherwise         = xs

multLOTByTrit :: Trit -> [Trit] -> [Trit]
multLOTByTrit Posi xs = xs
multLOTByTrit Zero xs = [Zero]
multLOTByTrit Nega xs = invertLOT xs

multLOT :: [Trit] -> [Trit] -> [Trit]
multLOT [] [] = [Zero]
multLOT xs [] = [Zero]
multLOT [] ys = [Zero]
multLOT xs ys = foldr addLOT [Zero] inlineProducts
    where inlineProducts = [unsafeShiftRLOT order value | (order, value) <- zip [0..] lnProds]
          lnProds        = [multLOTByTrit x ys | x <- xs]

exptLOT :: [Trit] -> Integer -> [Trit]
exptLOT x n
    | n > 0     = multLOT x (exptLOT x (n-1))
    | n == 0    = [Posi]
    | otherwise = throw NonTermination

trits2Int :: [Trit] -> Integer
trits2Int [] = 0
trits2Int xs = sum [d | d <- [trit2Int n x | (n, x) <- zip [0..] xs]]

-- can technically convert any positive integer, just inefficient
oneDigit2LOT :: Integer -> [Trit]
oneDigit2LOT n
    | n > 0     = posCarryLOT $ oneDigit2LOT (n-1)
    | n == 0    = []
    | otherwise = throw NonTermination

tenInLOT :: [Trit]
tenInLOT = [Posi,Zero,Posi]

digits2LOT :: Integer -> [[Trit]]
digits2LOT 0 = []
digits2LOT x = oneDigit2LOT (x `mod` 10) : digits2LOT (x `div` 10)

unsignedInt2LOT, int2LOT :: Integer -> [Trit]
unsignedInt2LOT x
    | x > 0     = foldr addLOT [] [multLOT order digit | (order, digit)
                            <- zip [exptLOT tenInLOT n | n <- [0..]] (digits2LOT x)]
    | x == 0    = [Zero]
    | otherwise = throw NonTermination

int2LOT x
    | x < 0     = invertLOT $ unsignedInt2LOT (-x)
    | x > 0     = unsignedInt2LOT x
    | otherwise = []

signumLOT       :: [Trit] -> Trit
signumLOT []    = Zero
signumLOT x     = last (chompLOT x)

absLOT      :: [Trit] -> [Trit]
absLOT x    = multLOT [signumLOT x] x

fixLenLOT           :: Int -> [Trit] -> [Trit]
fixLenLOT 0 []      = []
fixLenLOT 0 xs
    | isZeroes xs   = []
    | otherwise     = throw Overflow
fixLenLOT 1 [x]     = [x]
fixLenLOT n []      = Zero : fixLenLOT (n-1) []
fixLenLOT n (x:xs)  = x : fixLenLOT (n-1) xs

maxLOT, minLOT  :: Int -> [Trit]
maxLOT n
    | n == 0    = []
    | n > 0     = Posi : maxLOT (n-1)
    | otherwise = throw NonTermination

minLOT n
    | n == 0    = []
    | n > 0     = Nega : minLOT (n-1)
    | otherwise = throw NonTermination
{-
lot2Str :: [Trit] -> String
lot2Str = foldr ((:) . trit2Chr) ""
-}
-- eq class operations on trits
eqLOT, neqLOT                   :: [Trit] -> [Trit] -> Bool
[] `eqLOT` []                   = True
[] `eqLOT` [y]
    | y == Zero                 = True
    | otherwise                 = False
[x] `eqLOT` []
    | x == Zero                 = True
    | otherwise                 = False
[x] `eqLOT` [y]                 = x == y
[] `eqLOT` (y:ys)
    | y == Zero && isZeroes ys  = True
    | otherwise                 = False
(x:xs) `eqLOT` []
    | x == Zero && isZeroes xs  = True
    | otherwise                 = False
(x:xs) `eqLOT` (y:ys)           = x == y && xs `eqLOT` ys

xs `neqLOT` ys                  = not $ xs `eqLOT` ys

leLOT, ltLOT, geLOT, gtLOT      :: [Trit] -> [Trit] -> Bool
[] `ltLOT` []                   = False
[x] `ltLOT` [y]                 = x < y
[] `ltLOT` [y]
    | y == Posi                 = True
    | otherwise                 = False
[x] `ltLOT` []
    | x == Nega                 = True
    | otherwise                 = False
[] `ltLOT` ys
    | signumLOT ys == Posi      = True
    | otherwise                 = False
xs `ltLOT` []
    | signumLOT xs == Nega      = True
    | otherwise                 = False
xs `ltLOT` ys
    | length (chompLOT xs)
    == length (chompLOT ys)     = (last xs < last ys)
                                    || ((last xs == last ys)
                                        && (init xs `ltLOT` init ys))
    | length (chompLOT xs)
    < length (chompLOT ys)      = (signumLOT xs < signumLOT ys)
                                    || ((signumLOT xs == signumLOT ys)
                                        && (signumLOT xs == Posi))
    | otherwise                 = (signumLOT xs > signumLOT ys)
                                    || ((signumLOT xs == signumLOT ys)
                                        && (signumLOT xs == Nega))

xs `gtLOT` ys                   = not $ xs `ltLOT` ys

xs `leLOT` ys                   = xs `eqLOT` ys || xs `ltLOT` ys

xs `geLOT` ys                   = xs `eqLOT` ys || xs `gtLOT` ys

compareLOT                      :: [Trit] -> [Trit] -> Ordering
xs `compareLOT` ys
    | xs `ltLOT` ys             = LT
    | xs `eqLOT` ys             = EQ
    | otherwise                 = GT

andLOT, orLOT, xorLOT           :: [Trit] -> [Trit] -> [Trit]
xs `andLOT` ys                  = [x `andTrits` y | (x, y) <- zip xs ys]
xs `orLOT` ys                   = [x `orTrits` y | (x, y) <- zip xs ys]
xs `xorLOT` ys                  = [x `xorTrits` y | (x, y) <- zip xs ys]

rotateRLOT                      :: [Trit] -> Int -> [Trit]
rotateRLOT xs n
    | n > 0                     = rotateRLOT (last xs : init xs) (n-1)
    | n == 0                    = xs
    | otherwise                 = throw NonTermination

rotateLLOT                      :: [Trit] -> Int -> [Trit]
rotateLLOT xs n
    | n > 0                     = rotateLLOT (tail xs ++ [head xs]) (n-1)
    | n == 0                    = xs
    | otherwise                 = throw NonTermination

rotateLOT                       :: [Trit] -> Int -> [Trit]
rotateLOT xs n
    | n > 0                     = rotateLLOT xs n
    | n < 0                     = rotateRLOT xs n
    | otherwise                 = xs

-- definition of typeclass Trits
class (Integral a, Ix a) => Trits a where
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