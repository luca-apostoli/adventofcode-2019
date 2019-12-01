{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Data.ByteString hiding (hPutStrLn)
import Options.Generic
import System.Exit
import System.IO (stderr, hPutStrLn)
import Prelude
import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)
import qualified Day1 as Day1 (execute, parseInput)

data AOC w = AOC { 
        day :: w ::: Int <?> "Indicate which day to execute"
        , file :: w ::: String <?> "Indicate which file to use as input"
    } deriving (Generic)

instance ParseRecord (AOC Wrapped)
deriving instance Show (AOC Unwrapped)

main :: IO ()
main = do
    (AOC day file) <- unwrapRecord "Launch AOC"
    launch day file


readInput :: Stream String IO Char => String -> ParsecT String () IO [a] -> IO [a]
readInput file parser = do
                        input <- Prelude.readFile file
                        parsed <- runParserT parser () file input 
                        either report return parsed
                        where
                            report err = do
                                hPutStrLn stderr $ "Error: " ++ show err
                                exitFailure


launch :: Int -> String -> IO ()
launch 1 file = readInput file Day1.parseInput >>= Day1.execute
launch _ _ = undefined