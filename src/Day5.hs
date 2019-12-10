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


execute = undefined


parseInput :: Stream s m Char => ParsecT s u m [Instruction]
parseInput = manyTill instruction eof

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = Instruction 
                <$> parseModes
                <*> parseOperation
            where
                parseOpCode = try (string "01") <|> try (string "02") <|> try (string "03") <|> try (string "04") <|> try (string "99") <* char ','
                parseModes = upto 3 (modes <$> (char '0' <|> char '1')) <* try (lookAhead parseOpCode)
                parseOperation = do 
                                    op <- opCode <$> parseOpCode <* char ','
                                    pos <- option []  manyTill (upto1 digit <* char ',') (try (lookAhead parseOpCode))
                                    return op pos
                                    
                modes '1' = Immediate
                modes _ = Pointer

                opCode "01" = Sum
                opCode "02" = Prod
                opCode "03" = Input
                opCode "04" = Output
                opCode "99" = Halt
                opCode _ = error "unrecognized"



upto :: Stream s m Char => Int -> ParsecT s u m a -> ParsecT s u m [a]
upto n p | n > 0 = (:) <$> p <*> upto (n-1) p <|> return []
upto _ _ = return []

upto1 :: Stream s m Char => Int -> ParsecT s u m a -> ParsecT s u m [a]
upto1 n p | n > 0 = (:) <$> p <*> upto (n-1) p
upto1 _ _ = return []