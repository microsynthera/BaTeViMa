module BTWord where

import Trits

newtype BTWord18 = BTWord18 Trits deriving Show

instance Eq BTWord18 where
    (BTWord18 xs) == (BTWord18 ys) = xs `eqTrits` ys
    (BTWord18 xs) /= (BTWord18 ys) = xs `neqTrits` ys

instance Ord BTWord18 where

instance Num BTWord18 where

instance Real BTWord18 where

instance Enum BTWord18 where

instance Integral BTWord18 where

instance Bounded BTWord18 where

instance Bits BTWord18 where