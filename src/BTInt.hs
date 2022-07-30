module BTInt where

import Trits

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
    --complete this definition