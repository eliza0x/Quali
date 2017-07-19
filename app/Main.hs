module Main where

import Lexer
import qualified Data.Text.IO as T

main :: IO ()
main = mapM_ print . fromRight . lexer =<< T.getContents

fromRight :: (Show l) =>  Either l r -> r
fromRight (Right r) = r
fromRight (Left l) = error $ show l
