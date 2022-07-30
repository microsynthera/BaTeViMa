module Trits where

data Trit = Nega | Zero | Posi

instance Show Trit where
    show Nega = "-"
    show Zero = "0"
    show Posi = "+"

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
isNega _ = False

isZero :: Trit -> Bool
isZero Zero = True
isZero _ = False

isPosi :: Trit -> Bool
isPosi Posi = True
isPosi _ = False

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
    | a == b = invertTrit a
    | otherwise = error "added so hls won't complain"

carryTrit :: Trit -> Trit -> Trit
carryTrit a b
    | a == b = a
    | otherwise = Zero

addAndCarryTrit :: Trit -> Trit -> (Trit, Trit)
addAndCarryTrit a b = (addTrit a b, carryTrit a b)

--multTrit is just an alias for xnorTrit
multTrit :: Trit -> Trit -> Trit
multTrit = xnorTrit

trit2Int :: Int -> Trit -> Int
trit2Int x Nega = -1 * 3^x
trit2Int x Zero = 0 * 3^x
trit2Int x Posi = 1 * 3^x

{- "Trits" definitions
 - These definitions codify the behavior of lists of Trits
 - Lists of Trits are big endian (makes the recursion easier)
 -}

type Trits = [Trit]

isZeros :: Trits -> Bool
isZeros = foldr ((&&) . isZero) True

chompTrits :: Trits -> Trits
chompTrits [] = []
chompTrits (x:xs)
    | isZeros xs = [x]
    | otherwise = x : chompTrits xs

invertTrits :: Trits -> Trits
invertTrits = map invertTrit

posCarryTrits :: Trits -> Trits
posCarryTrits [] = [Posi]
posCarryTrits (x:xs)
    | x == Posi = Nega : posCarryTrits xs
    | otherwise = advanceTrit x : xs

negCarryTrits :: Trits -> Trits
negCarryTrits [] = [Nega]
negCarryTrits (x:xs)
    | x == Nega = Posi : negCarryTrits xs
    | otherwise = reverseTrit x : xs

addTrits :: Trits -> Trits -> Trits
addTrits [] [] = []
addTrits [] (y:ys) = y : ys
addTrits (x:xs) [] = x : xs
addTrits (x:xs) (y:ys)
    | isZero x && isZeros xs = y : ys
    | isZero y && isZeros ys = x : xs
    | carryTrit x y == Posi = addTrit x y : addTrits xs (posCarryTrits ys)
    | carryTrit x y == Nega = addTrit x y : addTrits xs (negCarryTrits ys)
    | otherwise = addTrit x y : addTrits xs ys

multTrits :: Trits -> Trits -> Trits
multTrits [] [] = []
multTrits xs [] = []
multTrits [] ys = []
-- TODO complete this definition

trits2Int :: Trits -> Int
trits2Int [] = 0
trits2Int xs = sum [d | d <- [trit2Int n x | (n, x) <- zip [0..] xs]]

newtype BTInt = BTInt [Trit]

instance Show BTInt where
    show (BTInt []) = ""
    show (BTInt (x:xs)) = show x ++ show (BTInt xs)

instance Eq BTInt where
    BTInt [] == BTInt [] = True
    BTInt (_:_) == BTInt [] = False
    BTInt [] == BTInt (_:_) = False
    BTInt (x:xs) == BTInt (y:ys)
        | isZeros xs && isZeros ys = x == y
        | otherwise = x == y && BTInt xs == BTInt ys

instance Ord BTInt where
    BTInt [] <= BTInt [] = True
    BTInt [x] <= BTInt [] = x <= Zero
    BTInt [] <= BTInt [x] = Zero <= x
    BTInt [x] <= BTInt [y] = x <= y
    BTInt (x:xs) <= BTInt [] = BTInt xs <= BTInt []
    BTInt [] <= BTInt (y:ys) = BTInt [] <= BTInt ys
    BTInt (x:xs) <= BTInt (y:ys)
        | isZeros xs && isZeros ys = x <= y
        | otherwise = BTInt xs <= BTInt ys

instance Enum BTInt where
    fromEnum (BTInt []) = 0
    fromEnum (BTInt xs) = trits2Int xs
    toEnum 0 = BTInt []
    toEnum x =
