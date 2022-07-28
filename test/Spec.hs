import Trits

tritsTestEq :: Bool
tritsTestEq =
    Posi == Posi &&
    Zero == Zero &&
    Nega == Nega &&
    Posi /= Nega &&
    Posi /= Zero &&
    Nega /= Zero

main :: IO ()
main = print "Test goes here."
