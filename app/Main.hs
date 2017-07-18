module Main where

import Lexer
import qualified Data.Text.IO as T

main :: IO ()
main = print . lexer =<< T.getContents
