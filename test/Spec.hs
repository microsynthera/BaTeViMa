{-}
import Trits ( Trit(..) )
import BTWord ( BTWord18, mkBTWord18 )

wordsArithSpec, wordsArithTest :: BTWord18
wordsArithSpec   = 0
wordsArithTest   = (-) 5050 $ sum [0..(10*10)]

wordsTritwiseSpec, wordsTritwiseTest :: BTWord18
wordsTritwiseSpec       = 0
wordsTritwiseTest       = 0 --need better test

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
    doTest "BTWord18 Arithmetic" wordsArithTest wordsArithSpec
    doTest "BTWord18 Tritwise" wordsTritwiseTest wordsTritwiseSpec
    putStrLn "All tests complete!"
-}