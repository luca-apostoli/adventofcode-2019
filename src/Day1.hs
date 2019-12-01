{-# LANGUAGE FlexibleContexts  #-}

module Day1 
    (
        execute
        , parseInput
    ) where

import Prelude
import Text.Parsec
import Text.Parsec.Char


execute :: [String] -> IO ()
execute l = print $ calculate l
            where                
                calculate = foldl (\acc x -> acc + fuel (read x :: Double)) 0

fuel :: Double -> Integer
fuel x = (flip (-) 2) $ floor ( x / 3)


parseInput :: Stream s m Char => ParsecT s u m [String]
parseInput = (many digit) `sepBy` (char '\n')