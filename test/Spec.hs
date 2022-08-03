import Trits ( Trit(..) )
import BTWord ( BTWord18, Trits(..) )

wordsArithSpec, wordsArithTest :: BTWord18
wordsArithSpec   = 0
wordsArithTest   = (-) 123 $ (*) 12 10 + 3

wordsTritwiseSpec, wordsTritwiseTest :: BTWord18
wordsTritwiseSpec = 59048
wordsTritwiseTest = setTrit (setTrit 0 Posi 10) Nega 0

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