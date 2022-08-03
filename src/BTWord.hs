module BTWord where

import Trits

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

newtype BTWord18 = BTWord18 Trits.Trits deriving Show

--instance Show BTWord18 where
--    show (BTWord18 xs)                      = "0t" ++ trits2Str xs

instance Eq BTWord18 where
    (BTWord18 xs) == (BTWord18 ys)          = xs `eqTrits` ys
    (BTWord18 xs) /= (BTWord18 ys)          = xs `neqTrits` ys

instance Ord BTWord18 where
    (BTWord18 xs) `compare` (BTWord18 ys)   = xs `compareTrits` ys

instance Num BTWord18 where
    (BTWord18 xs) + (BTWord18 ys)           = BTWord18 $ fixLenTrits 18 $ xs `addTrits` ys
    negate (BTWord18 xs)                    = BTWord18 $ invertTrits xs
    abs (BTWord18 xs)                       = BTWord18 $ absTrits xs
    signum (BTWord18 xs)                    = BTWord18 [signumTrits xs]
    (BTWord18 xs) * (BTWord18 ys)           = BTWord18 $ fixLenTrits 18 $ xs `multTrits` ys
    fromInteger xs                          = BTWord18 $ fixLenTrits 18 $ int2Trits xs

instance Enum BTWord18 where
    fromEnum (BTWord18 xs)                  = fromIntegral $ trits2Int xs
    toEnum xs                               = BTWord18 $ int2Trits $ fromIntegral xs

instance Bounded BTWord18 where
    minBound                                = BTWord18 $ minTrits 18
    maxBound                                = BTWord18 $ maxTrits 18

instance BTWord.Trits BTWord18 where
    (BTWord18 xs) .&. (BTWord18 ys)         = BTWord18 $ xs `andTrits` ys
    (BTWord18 xs) .|. (BTWord18 ys)         = BTWord18 $ xs `orTrits` ys
    (BTWord18 xs) `xor` (BTWord18 ys)       = BTWord18 $ xs `xorTrits` ys
    complement (BTWord18 xs)                = BTWord18 $ invertTrits xs
    shift (BTWord18 xs) n                   = BTWord18 $ shiftTrits xs n
    rotate (BTWord18 xs) n                  = BTWord18 $ rotateTrits xs n
    tritSize (BTWord18 xs)                  = 18
    testTrit (BTWord18 xs) n                = last $ take (n+1) xs
    setTrit (BTWord18 xs) a n               = BTWord18 $ take n xs ++ [a] ++ drop (n+1) xs

-- exported definitions
readTrit :: Char -> Trit
readTrit '+'        = Posi
readTrit '0'        = Zero
readTrit '-'        = Nega
readTrit _          = undefined
