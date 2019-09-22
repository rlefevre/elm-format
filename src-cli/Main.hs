{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified ElmFormat
import qualified System.Environment


main :: IO ()
main =
    do
        args <- System.Environment.getArgs
        ElmFormat.main args
