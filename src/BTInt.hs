module BTInt where

import Trits

newtype BTInt = BTInt [Trit] deriving Show

instance Eq BTInt where
    BTInt [] == BTInt [] = True
    BTInt [] == BTInt [y]
        | y == Zero = True
        | otherwise = False
    BTInt [x] == BTInt []
        | x == Zero = True
        | otherwise = False
    BTInt [x] == BTInt [y] = x == y
    BTInt [] == BTInt (y:ys)
        | y == Zero && isZeros ys = True
        | otherwise = False
    BTInt (x:xs) == BTInt []
        | x == Zero && isZeros xs = True
        | otherwise = False
    BTInt (x:xs) == BTInt (y:ys) = x == y && BTInt xs == BTInt ys

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
    fromEnum (BTInt [Zero]) = 0
    fromEnum (BTInt xs) = fromIntegral (trits2Int xs)
    toEnum 0 = BTInt [Zero]
    toEnum x = BTInt (chompTrits (int2Trits (fromIntegral x)))

instance Num BTInt where
    (BTInt x) + (BTInt y) = BTInt (addTrits x y)
    (BTInt x) * (BTInt y) = BTInt (multTrits x y)
    negate (BTInt x) = BTInt (invertTrits x)
    signum (BTInt x) = BTInt [signumTrits x]
    fromInteger x = BTInt (chompTrits (int2Trits x))
    abs (BTInt x) 
        | null x = BTInt [Zero]
        | signumTrits x == Nega = BTInt (invertTrits x)
        | signumTrits x == Zero = BTInt [Zero]
        | signumTrits x == Posi = BTInt x
        | otherwise = error "failed to apply abs on BTInt"

instance Integral BTInt where
    -- complete this instance