module Main where

import Lib ( waveField )

main :: IO ()
main = print (waveField 24 24)
