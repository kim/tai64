module Main (main) where

import Test.DocTest


main :: IO ()
main = doctest [ "-isrc", "src/Data/Time/Clock/TAI64.hs" ]
