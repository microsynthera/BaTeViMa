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

advanceTrit :: Trit -> Trit
advanceTrit Nega = Zero
advanceTrit Zero = Posi
advanceTrit Posi = Nega

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

{- the final pair's first element is the result trit, 
 - the last element is the carry trit
 -}
addTrit :: Trit -> Trit -> (Trit, Trit)
addTrit Zero Zero = (Zero, Zero)
addTrit Zero x = (Zero, x)
addTrit Posi x
    | advanceTrit x == Nega = (Posi, Nega)
    | otherwise = (Zero, advanceTrit x)
addTrit Nega x
    | reverseTrit x == Posi = (Nega, Posi)
    | otherwise = (Zero, reverseTrit x)

{- "Trits" definitions 
 - These definitions codify the behavior of lists of Trits
 - Lists of Trits are big endian (makes the recursion easier)
 -}

type Trits = [Trit]

isZeros :: Trits -> Bool
isZeros = foldr ((&&) . isZero) True

invertTrits :: Trits -> Trits
invertTrits = map invertTrit

posiCarryTrits :: Trits -> Trits

negaCarryTrits :: Trits -> Trits

addTrits :: Trits -> Trits -> Trits
addTrits [] [] = []
addTrits [] (y:ys) = y : addTrits [] ys
addTrits (x:xs) [] = x : addTrits xs []
addTrits (x:xs) (y:ys)
    | isZero x && isZeros xs = y : ys
    | isZero y && isZeros ys = x : xs
    | otherwise = 
        let interResult = addTrit x y
            in last interResult 
