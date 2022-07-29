import Trits

testTritsEq :: Bool
testTritsEq =
    Posi == Posi &&
    Zero == Zero &&
    Nega == Nega

testTritsOrd :: Bool
testTritsOrd =
    Posi <= Posi &&
    Zero <= Posi &&
    Zero <= Zero &&
    Nega <= Posi &&
    Nega <= Zero &&
    Nega <= Nega

testTritsInv :: Bool
testTritsInv =
    Posi == invertTrit Nega &&
    Nega == invertTrit Posi &&
    Zero == invertTrit Zero



main :: IO ()
main = print "Test goes here."
