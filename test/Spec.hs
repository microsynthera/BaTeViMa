import Trits

addAndCarryTritSpec :: [(Trit, Trit)]
addAndCarryTritSpec = [ (Posi, Nega), (Nega, Zero), (Zero, Zero)
                      , (Nega, Zero), (Zero, Zero), (Posi, Zero)
                      , (Zero, Zero), (Posi, Zero), (Nega, Posi) ]

addAndCarryTritTest :: [(Trit, Trit)]
addAndCarryTritTest = 
    [ addAndCarryTrit a b | a <- [Nega, Zero, Posi]
                          , b <- [Nega, Zero, Posi] ]

posCarryTritsSpec :: Trits
posCarryTritsSpec = [Nega, Nega, Nega, Nega, Posi]

posCarryTritsTest :: Trits
posCarryTritsTest = posCarryTrits (posCarryTrits [Zero, Posi, Posi, Posi])

negCarryTritsSpec :: Trits
negCarryTritsSpec = [Posi, Posi, Posi, Posi, Nega]

negCarryTritsTest :: Trits
negCarryTritsTest = negCarryTrits (negCarryTrits [Zero, Nega, Nega, Nega])

addTritsSpec :: Trits
addTritsSpec = [Zero]

addTritsTest :: Trits
addTritsTest =
    chompTrits
        (addTrits [Posi, Posi, Posi]
            (addTrits [Posi] 
                (addTrits [Zero, Posi, Nega] [Posi, Zero, Nega])))

doTest :: (Eq a, Show a) => String -> a -> a -> IO()
doTest name test spec = 
    if test == spec
        then do putStr name
                putStrLn " works! :)" 
        else do putStr name
                putStrLn " is broken! :("
                putStr "Expected:  "
                print spec
                putStr ", but Got: "
                print test

main :: IO ()
main = do
    doTest "addAndCarryTrit" addAndCarryTritTest addAndCarryTritSpec
    doTest "posCarryTrits" posCarryTritsTest posCarryTritsSpec
    doTest "negCarryTrits" negCarryTritsTest negCarryTritsSpec
    doTest "addTrits" addTritsTest addTritsSpec
    putStrLn "All tests complete!"