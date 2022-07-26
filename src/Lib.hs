module Lib
    ( waveField
    ) where

{- Miscellaneous Definitions for Fun :) -}

someFunc :: Double -> Double -> (Double, Double, Double)
someFunc x y = (x, y, sin x + cos y)

divPi :: Double -> Double
divPi = (/pi)

someAngles :: Int -> [Double]
someAngles n = [fromIntegral x * pi / 12.0 | x <- [0..n]]

waveField :: Int -> Int -> [[(Double, Double, Double)]]
waveField xlen ylen =
    [[someFunc x y | x <- someAngles xlen] | y <- someAngles ylen]

rightTris :: Int -> [(Int, Int, Int)]
rightTris n =
    [(a,b,c)
      | c <- [1..n]
      , b <- [1..c]
      , a <- [1..b]
      , a^2 + b^2 == c^2]

someSeries :: Integer -> Integer
someSeries 0 = 0
someSeries 1 = 1
someSeries x
    | x < 0 = error "undefined"
    | otherwise = someSeries (x - 1) + someSeries (x - 2)

{- Balanced Ternary Definitions -}

isTrit :: Int -> Bool
isTrit x
    | (-1) <= x && x <= 1 = True
    | otherwise = False

b3Not :: Int -> Int
b3Not a = -a

b3And :: Int -> Int -> Int
b3And (-1) x  = -1
b3And x (-1)  = -1
b3And ( 0) x  = 0
b3And x ( 0)  = 0
b3And (1) (1) = 1
b3And a b = error "not trits"

b3Or :: Int -> Int -> Int
b3Or ( 1) x    = 1
b3Or x ( 1)    = 1
b3Or ( 0) x    = 0
b3Or x ( 0)    = 0
b3Or (-1) (-1) = -1
b3Or a b = error "not trits"

b3Nand :: Int -> Int -> Int
b3Nand a b = b3Not (b3And a b)

b3Nor :: Int -> Int -> Int
b3Nor a b = b3Not (b3Or a b)

{- Old Adder Logic -
b3CarryLogic :: Int -> Int -> Int
b3CarryLogic a b
    | isTrit a && isTrit b = case (a+b) of (-2) -> (-1)
                                           (-1) -> ( 0)
                                           ( 0) -> ( 0)
                                           ( 1) -> ( 0)
                                           ( 2) -> ( 1)
    | otherwise = error "undefined"

b3AddLogic :: Int -> Int -> Int
b3AddLogic a b
    | isTrit a && isTrit b = case (a+b) of (-2) -> ( 1)
                                           (-1) -> (-1)
                                           ( 0) -> ( 0)
                                           ( 1) -> ( 1)
                                           ( 2) -> (-1)
    | otherwise = error "undefined"

b3Adder :: Int -> Int -> (Int, Int)
b3Adder a b
    | isTrit a && isTrit b = (b3CarryLogic a b, b3AddLogic a b)
    | otherwise = error "undefined"
-}
