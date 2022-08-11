module BTInteger where

import Trits
    ( Trits((.&.), setTrit, testTrit, tritSize, rotate, shift,
            complement, xor, (.|.)),
      Trit,
      trits2Int,
      chompLOT,
      invertLOT,
      addLOT,
      multLOT,
      int2LOT,
      signumLOT,
      absLOT,
      eqLOT,
      neqLOT,
      compareLOT,
      andLOT,
      orLOT,
      xorLOT,
      rotateLOT,
      unsafeShiftLOT )

import Data.Ix ( Ix )

newtype BTInteger = BTInteger [Trit] deriving Show

instance Trits BTInteger where
    (BTInteger xs) .&. (BTInteger ys)   = BTInteger $ xs `andLOT` ys
    (BTInteger xs) .|. (BTInteger ys)   = BTInteger $ xs `orLOT` ys
    (BTInteger xs) `xor` (BTInteger ys) = BTInteger $ xs `xorLOT` ys
    complement (BTInteger xs)           = BTInteger $ invertLOT xs
    shift (BTInteger xs) n              = BTInteger $ unsafeShiftLOT xs n
    rotate (BTInteger xs) n             = BTInteger $ rotateLOT xs n
    tritSize (BTInteger xs)             = length $ chompLOT xs
    testTrit (BTInteger xs) n           = last $ take (n+1) xs
    setTrit (BTInteger xs) a n          = BTInteger $ take n xs ++ [a] ++ drop (n+1) xs

instance Integral BTInteger where

instance Real BTInteger where

instance Num BTInteger where
    (BTInteger xs) + (BTInteger ys)     = BTInteger $ xs `addLOT` ys
    negate (BTInteger xs)               = BTInteger $ invertLOT xs
    abs (BTInteger xs)                  = BTInteger $ absLOT xs
    signum (BTInteger xs)               = BTInteger [signumLOT xs]
    (BTInteger xs) * (BTInteger ys)     = BTInteger $ xs `multLOT` ys
    fromInteger xs                      = BTInteger $ int2LOT xs

instance Enum BTInteger where
    fromEnum (BTInteger xs)             = fromIntegral $ trits2Int xs
    toEnum xs                           = BTInteger $ int2LOT $ fromIntegral xs

instance Ord BTInteger where
    (BTInteger xs) `compare` (BTInteger ys) = xs `compareLOT` ys

instance Eq BTInteger where
    (BTInteger xs) == (BTInteger ys)    = xs `eqLOT` ys
    (BTInteger xs) /= (BTInteger ys)    = xs `neqLOT` ys

instance Ix BTInteger where
