{-# LANGUAGE FlexibleContexts  #-}

module Day2 (
    execute
    , parseInput
) where

import Prelude
import Data.List
import Text.Parsec
import Text.Parsec.Char


data MachineStatus = Running [Integer] | Alt [Integer]

execute :: [String] -> IO ()
execute input = do
--            let res = transformByOpCode 0 $ setup 2 12 $ map (\x -> read x :: Integer) input
--            print $ head res
              let res = findTarget 19690720 $ map (\x -> read x :: Integer) input
              print (100 * fst res + snd res)


findTarget :: Integer -> [Integer] -> (Integer, Integer)
findTarget target input = let
                             tuples = [(x, y) | x <- [0..99], y <- [0..99]]
                             res = map (\(x, y) -> transformByOpCode 0 $ setup x y input) tuples
                             found = head $ filter (\x -> target == genericIndex x 0) res
                          in
                             (genericIndex found 1, genericIndex found 2)


setup :: Integer -> Integer -> [Integer] -> [Integer]
setup noun verb input = replaceNth 2 verb $ replaceNth 1 noun input

transformByOpCode :: Integer -> [Integer] -> [Integer]
transformByOpCode _ [] = []
transformByOpCode index input | index >= genericLength input = input
                              | otherwise = let
                                    op = genericIndex input index
                                    pos = (
                                            genericIndex input (index + 1)
                                            ,genericIndex input (index + 2)
                                            , genericIndex input (index + 3)
                                            )
                                in
                                    case opcode op pos input of
                                        Running changed -> transformByOpCode (index + 4) changed
                                        Alt changed -> changed 



opcode :: Integer -> (Integer, Integer, Integer) -> [Integer] -> MachineStatus
opcode 99 _ xs = Alt xs
opcode 1 pos xs = Running $ calculateOp (+) pos xs
opcode 2 pos xs = Running $ calculateOp (*) pos xs
opcode _ _ _ = Alt []


calculateOp :: (Integer -> Integer -> Integer) -> (Integer, Integer, Integer) -> [Integer] -> [Integer]
calculateOp op (p_a1, p_a2, p_r) xs = let 
                                        a1 = genericIndex xs p_a1
                                        a2 = genericIndex xs p_a2
                                    in
                                        replaceNth p_r (a1 `op` a2) xs


replaceNth :: Integer -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
            | n == 0 = newVal : xs
            | otherwise = x : replaceNth (n-1) newVal xs


parseInput :: Stream s m Char => ParsecT s u m [String]
parseInput = many digit `sepBy` char ','
