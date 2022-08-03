import BTWord

wordsSpec :: BTWord18
wordsSpec   = 0

wordsTest :: BTWord18
wordsTest   = 123 - 123

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
    doTest "BTWord18" wordsTest wordsSpec
    putStrLn "All tests complete!"