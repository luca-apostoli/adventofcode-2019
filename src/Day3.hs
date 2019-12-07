{-# LANGUAGE FlexibleContexts  #-}

module Day3 (
      execute
    , parseInput
    , init
    , move 
) where

import Prelude hiding (init)
import Data.List hiding (init)
import Text.Parsec hiding (Empty)
import Text.Parsec.Char
import Control.Monad
import qualified Data.Map.Strict as Map


data Direction = R | U | L | D deriving (Eq, Show, Read)
data Movement = Movement Direction Int deriving (Eq, Show)
type PlayerName = Int
type Step = Int
data Box = Player PlayerName Step | Cross (Step, Step) | Empty| Start deriving (Eq, Show)
type Board = Map.Map Int (Map.Map Int Box)
data Position = Position { 
                      player :: PlayerName
                    , pos_row :: Int
                    , pos_col :: Int
                    , step :: Step
                    } deriving (Eq, Show)


execute :: [[Movement]] -> IO ()
execute l = let 
                board1 = foldl (flip move) (init 1) (head l)
                board2 = foldl (flip move) (Position 2 0 0 0, snd board1) (l !! 1)
            in 
                -- part one
                -- pPrint $ minimum . foldr calculateManhattanDistance [] $ Map.toAscList $ Map.map Map.keys $ findCrossInBoard $ snd board2
                -- part two
                print $ minimum . concat . Map.elems $ Map.map Map.elems $ Map.map findStepsToCrossInRow  $ snd board2 


init :: PlayerName -> (Position, Board)
init p = (Position p 0 0 0, Map.singleton 0 (Map.singleton 0 Start))


emptyRow :: Map.Map Int Box
emptyRow = Map.singleton 0 Empty

calculateManhattanDistance :: (Int, [Int]) -> [Int] -> [Int]
calculateManhattanDistance (p, qs) dist = foldr (\x acc -> (abs p + abs x) : acc) dist qs


findCrossInBoard :: Board -> Map.Map Int (Map.Map Int Int)
findCrossInBoard = Map.filter (not . Map.null) . Map.map findCrossInRow

findCrossInRow :: Map.Map Int Box -> Map.Map Int Int
findCrossInRow = Map.mapMaybeWithKey keepCrossIndex
                    where 
                        keepCrossIndex k (Cross _) = Just k
                        keepCrossIndex _ _ = Nothing

findStepsToCrossInRow :: Map.Map Int Box -> Map.Map Int Int
findStepsToCrossInRow = Map.mapMaybe keepCrossSteps
                    where 
                        keepCrossSteps (Cross (s1, s2)) = Just $ s1 + s2 - 2
                        keepCrossSteps _ = Nothing


move :: Movement -> (Position, Board) -> (Position, Board)
move m@(Movement L pos) (current, b) = (updatePosition m current, left pos current b)
move m@(Movement R pos) (current, b) = (updatePosition m current, right pos current b)
move m@(Movement U pos) (current, b) = (updatePosition m current, up pos current b)
move m@(Movement D pos) (current, b) = (updatePosition m current, down pos current b)


left :: Int -> Position -> Board -> Board
left cells current b = let 
                            b_row = Map.findWithDefault emptyRow (pos_row current) b
                            line = [(pos_col current - cells)..(pos_col current)]
                            changed = foldl' (\acc x -> Map.alter (changeBox (player current) (getStep current x)) x acc) b_row line
                        in
                            Map.alter (\_ -> Just changed) (pos_row current) b
                        where 
                            getStep current rowIdx = abs (rowIdx - pos_col current) + step current + 1

right :: Int -> Position -> Board -> Board
right cells current b = let 
                            b_row = Map.findWithDefault emptyRow (pos_row current) b
                            line = [(pos_col current)..(pos_col current + cells)]
                            changed = foldl' (\acc x -> Map.alter (changeBox (player current) (getStep current x)) x acc) b_row line
                        in
                            Map.alter (\_ -> Just changed) (pos_row current) b
                        where 
                            getStep current rowIdx = abs (rowIdx - pos_col current) + step current + 1
                            
up :: Int -> Position -> Board -> Board
up cells current b = let 
                            line = [(pos_row current)..(pos_row current + cells)]
                            changed_row s Nothing = Just $ Map.alter (changeBox (player current) s) (pos_col current) emptyRow
                            changed_row s (Just row) = Just $ Map.alter (changeBox (player current) s) (pos_col current) row                            
                        in
                            foldl' (\acc x -> Map.alter (changed_row (getStep current x)) x acc) b line
                        where 
                            getStep current rowIdx = abs (rowIdx - pos_row current) + step current + 1


down :: Int -> Position -> Board -> Board
down cells current b = let 
                            line = [(pos_row current - cells)..(pos_row current)]
                            changed_row s Nothing = Just $ Map.alter (changeBox (player current) s) (pos_col current) emptyRow
                            changed_row s (Just row) = Just $ Map.alter (changeBox (player current) s) (pos_col current) row                            
                        in
                            foldl' (\acc x -> Map.alter (changed_row (getStep current x)) x acc) b line
                        where 
                            getStep current rowIdx = abs (rowIdx - pos_row current) + step current + 1



updatePosition :: Movement -> Position -> Position
updatePosition (Movement L pos) current = Position (player current) (pos_row current) (pos_col current - pos) (step current + pos)
updatePosition (Movement R pos) current = Position (player current) (pos_row current) (pos_col current + pos) (step current + pos)
updatePosition (Movement U pos) current = Position (player current) (pos_row current + pos) (pos_col current) (step current + pos)
updatePosition (Movement D pos) current = Position (player current) (pos_row current - pos) (pos_col current) (step current + pos)


changeBox :: PlayerName -> Step -> Maybe Box -> Maybe Box
changeBox p s Nothing = Just $ Player p s
changeBox p s (Just Empty) = Just $ Player p s
changeBox p s (Just (Cross s2)) = Just $ Cross $ mergeCrossedSteps p s2 s
changeBox _ _ (Just Start) = Just Start
changeBox p s (Just (Player p2 s2)) 
                    | p == p2 = Just $ Player p s
                    | p == 1 = Just $ Cross (s, s2)
                    | otherwise = Just $ Cross (s2, s)

mergeCrossedSteps :: PlayerName -> (Step, Step) -> Step -> (Step, Step)
mergeCrossedSteps p (s1, s2) s 
                            | p == 1 && s1 == 0 = (s, s2)
                            | p == 1 = (min s s1, s2) 
                            | s2 == 0 = (s1, s)
                            | otherwise = (s1, min s s2)

parseInput :: Stream s m Char => ParsecT s u m [[Movement]]
parseInput = movement `sepBy` char ',' `sepBy` char '\n'

movement :: Stream s m Char => ParsecT s u m Movement
movement = Movement
                <$> (direction <$> (char 'R' <|> char 'U' <|> char 'L' <|> char 'D'))
                <*> (distance <$> many digit)
            where 
                direction :: Char -> Direction
                direction x = read [x] :: Direction
                distance x = read x :: Int