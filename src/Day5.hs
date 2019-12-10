{-# LANGUAGE FlexibleContexts  #-}

module Day5 (
    execute
    , parseInput
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

execute :: [Instruction] -> IO ()
execute = print 


parseInput :: Stream s m Char => ParsecT s u m [Instruction]
parseInput = manyTill instruction eof

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = Instruction 
                <$> (try (parseModes 3) <|> try (parseModes 2) <|> try (parseModes 1) <|> return [])  
                <*> parseOperation
            where                                                
                parseModes n = count n (modes <$> (char '0' <|> char '1')) <* try (lookAhead parseOpCode)
                parseOpCode = try (string "01") <|> try (string "02") <|> try (string "03") <|> try (string "04") <|> try (string "99") <* (char ',' <|> char '\n')
                parseOperation = do 
                                    op <- parseOpCode
                                    readPos <- pos $ posNum op
                                    return $ opCode op $ map (\x -> read x :: Int) readPos
                                    where 
                                        posNum "01" = 3
                                        posNum "02" = 3
                                        posNum "03" = 1
                                        posNum "04" = 1
                                        posNum _ = 0
                                                    
                                        
                                        

pos :: Stream s m Char => Int -> ParsecT s u m [String]
pos n = count n (many digit <* char ',')


modes :: Char -> Mode
modes '1' = Immediate
modes _ = Pointer


opCode :: String -> [Int] -> Operation
opCode "01" xs = Sum (xs !! 0) (xs !! 1) (xs !! 2)
opCode "02" xs = Prod (xs !! 0) (xs !! 1) (xs !! 2)
opCode "03" xs = Input (xs !! 0)
opCode "04" xs = Output (xs !! 0)
opCode "99" _ = Halt
opCode _ _ = error "unrecognized"



upto :: Stream s m Char => Int -> ParsecT s u m a -> ParsecT s u m [a]
upto n p | n > 0 = (:) <$> p <*> upto (n-1) p <|> return []
upto _ _ = return []

upto1 :: Stream s m Char => Int -> ParsecT s u m a -> ParsecT s u m [a]
upto1 n p | n > 0 = (:) <$> p <*> upto (n-1) p
upto1 _ _ = return []