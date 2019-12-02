{-# LANGUAGE FlexibleContexts  #-}

module Day1 
    (
        execute
        , parseInput
        , amount
        , fuel
    ) where

import Prelude
import Text.Parsec
import Text.Parsec.Char


execute :: [String] -> IO ()
execute l = print $ calculate l
            where                
                calculate = foldl (\acc x -> acc + sum (amount [] (read x :: Integer))) 0

amount :: [Integer] -> Integer -> [Integer]
amount tot x | fuel x <= 0      = tot
             | otherwise        = amount (fuel x : tot) (fuel x) 

fuel :: Integer -> Integer
fuel x = (flip (-) 2) $ ( x `div` 3)


parseInput :: Stream s m Char => ParsecT s u m [String]
parseInput = (many digit) `sepBy` (char '\n')