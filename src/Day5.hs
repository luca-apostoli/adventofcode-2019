{-# LANGUAGE FlexibleContexts  #-}

module Day5 (
    execute
    , parseInput
    , parseInstruction
) where

import Prelude hiding (init)
import Data.List hiding (init)
import Text.Parsec hiding (Empty)
import Text.Parsec.Char
import Control.Monad



data Mode = Immediate | Pointer deriving (Eq, Show)
data Operation = Sum Int Int Int | Prod Int Int Int | Input Int | Output Int | Halt deriving (Eq, Show)
data Instruction = Instruction {
                      mode :: [Mode]
                    , operation :: Operation
                } deriving (Eq, Show)

execute :: [Int] -> [Instruction] -> IO ()
execute s ins = print ins


exec :: Instruction -> [Int] -> IO [Int]
exec (Instruction m o@Sum{}) = execSum m o
exec (Instruction m o@Prod{}) = execProd m o
exec (Instruction m o@Input{}) = execInput m o
exec (Instruction m o@Output{}) = execOutput m o
exec _ = return


execSum :: [Mode] -> Operation -> [Int] -> IO [Int]
execSum ms (Sum a b c) xs = return $ setValue c (s0 + s1) xs
                    where 
                        s0 = getValue (readMode ms 0) xs a
                        s1 = getValue (readMode ms 1) xs b

execProd :: [Mode] -> Operation -> [Int] -> IO [Int]
execProd ms (Prod a b c) xs = return $ setValue c (p0 * p1) xs
                        where 
                            p0 = getValue (readMode ms 0) xs a
                            p1 = getValue (readMode ms 1) xs b

execInput :: [Mode] -> Operation -> [Int] -> IO [Int]
execInput ms (Input a) xs = do
                            val <- readLn
                            return $ setValue a val xs

execOutput :: [Mode] -> Operation -> [Int] -> IO [Int]
execOutput ms (Output a) xs = do
                            print $ getValue Pointer xs a
                            return xs


readMode :: [Mode] -> Int -> Mode
readMode [] _ = Pointer
readMode ms i | i >= 3 || i < 0 || i > length ms = Pointer
              | otherwise = ms !! i


getValue :: Mode -> [Int] -> Int -> Int
getValue Immediate _ v = v
getValue Pointer xs v = xs !! v

setValue :: Int -> Int -> [Int] -> [Int]
setValue _ _ [] = []
setValue n newVal (x:xs)
            | n == 0 = newVal : xs
            | otherwise = x : setValue (n-1) newVal xs


parseInput :: Stream s m Char => ParsecT s u m [Int]
parseInput = map (read :: String -> Int) <$> many digit `sepBy` char ','

parseInstruction :: Stream s m Char => ParsecT s u m [Instruction]
parseInstruction = manyTill instruction eof

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = Instruction 
                <$> (try (parseModes 3) <|> try (parseModes 2) <|> try (parseModes 1) <|> return [])  
                <*> parseOperation
            where                                                
                parseModes n = count n (modes <$> (char '0' <|> char '1')) <* try (lookAhead parseOpCode)
                parseOpCode = try (string "01") <|> try (string "02") <|> try (string "03") <|> try (string "04") <|> try (string "99") <* option ',' (char ',' <|> char '\n')
                parseOperation = do 
                                    op <- parseOpCode
                                    _ <- manyTill anyChar (option ',' (char ',' <|> char '\n'))
                                    readPos <- pos $ posNum op
                                    return $ opCode op $ map (read :: String -> Int) readPos
                                    where 
                                        posNum "01" = 3
                                        posNum "02" = 3
                                        posNum "03" = 1
                                        posNum "04" = 1
                                        posNum _ = 0
                                                    
                                        
                                        

pos :: Stream s m Char => Int -> ParsecT s u m [String]
pos n = count n (many digit <* (char ',' <|> char '\n'))


modes :: Char -> Mode
modes '1' = Immediate
modes _ = Pointer


opCode :: String -> [Int] -> Operation
opCode "01" xs = Sum (head xs) (xs !! 1) (xs !! 2)
opCode "02" xs = Prod (head xs) (xs !! 1) (xs !! 2)
opCode "03" xs = Input (head xs)
opCode "04" xs = Output (head xs)
opCode "99" _ = Halt
opCode _ _ = error "unrecognized"
