-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module ReversiAI(State,author,nickname,initial,think) where

import Reversi
import Data.Maybe
import Data.List
import Data.Sequence (fromList,update)
import Data.Foldable (toList)
import System.Random
import Data.Ord (comparing)


-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\
{-
https://arttuys.fi/texts/2017/haskellversi I have used this article for some inspiration regarding discovery. 
Basically realized that it is better to use directions even though it went a bit tricky with 0..63 positions
State stores the next player who will take the move and the board.
Cell is a game sqaure, has three states Black, White or Nothing
I have tried implemented logic for better move decisions using some playing suggestions from the web
I have spent some time figuring out how to deal with swapping turns correctly, but seems like now it looks logical
-}
-- Remember to provide a brief (about 100-500 words) description of
-- your implementation.


author :: String
author = "Mikhail Boronin"

nickname :: String
nickname = "mboronin"

data Cell =
  Cell Int (Maybe Player)
  deriving (Eq, Show)
-- Board = [Int -> (Maybe Player)]
type Board = [Cell]
-- current board and current player
type State = (Board, Player)

initCell :: Int -> Cell
initCell n | n == 27 || n == 36 = Cell (n) (Just White)
           | n == 28 ||n == 35 = Cell (n) (Just Black)
           | otherwise = Cell (n) Nothing

-- creates a board of cells           
initialBoard :: Board
initialBoard = [initCell n | n <- [0..63]]

-- returns initiails state with the player that is about to make a move. since think is launched by game, opponent always goes first (with Pass or normal move)
initial :: Reversi.Player -> State
initial player = (initialBoard, swapPlayer player)

-- 1. Receive the opponents move, adjust the state
-- 2. Select your new move based on the board
-- 3. Adjust the state with the selected move, inverse the player inside the state
-- 4. Return new move and current state (from 3)
think :: State -> Reversi.Move -> Double -> (Reversi.Move,State)
think state@(board, opponent) move time = (myMove, newState)
                  where  
                  midState@(board', player') = updateState state move 
                  myMove | time > 10.0 = bestMove board' player'
                         | otherwise = randomMove board' player'
                  newState = updateState midState myMove        

-- state has the current player
updateState :: State -> Reversi.Move -> State
updateState state@(board, player) move
                              | move == Pass = (board,  nextPlayer)
                              | otherwise = (applyMove board (Just player) (movesFromCell board player move), nextPlayer)
                                where nextPlayer = swapPlayer player

-- swaps the players
swapPlayer :: Player -> Player
swapPlayer Black = White
swapPlayer White = Black


-- Returns a player by position
playerAtCell :: Board -> Int -> (Maybe Player)
playerAtCell ((Cell pos player):xs) n | pos /= n = playerAtCell xs n
                                    | pos == n = player

-- Returns cell by number                                     
showCell :: Board -> Int -> Cell
showCell ((Cell pos player):xs) n | pos /= n = showCell xs n
                                  | pos == n = (Cell pos player)

-- updates the cell inside the board with position index and player type
updateCell :: Board -> Maybe Player -> Int -> Board
updateCell list player' n = toList . update n (Cell n player') $ fromList list

-- returns a boolean value where the cell is occupied or not
isOccupied :: Board -> Int -> Bool 
isOccupied ((Cell pos player):xs) n | pos /= n = isOccupied xs n
                                    | pos == n = player == Nothing

-- returns True if the cells are owned by the same player
playerAtCellEq :: Cell -> Cell -> Bool
playerAtCellEq (Cell _ p) (Cell _ p') = p == p'

-- checks if the coordinate is in bounds
insideTheBoard :: Int -> Bool
insideTheBoard n | n >= 0, n < 64 = True
                     | otherwise = False

-- returns the list of cells to be changed if the move is made
-- Cell must be empty and a valid coordinate
movesFromCell :: Board -> Player -> Move-> [Int]
movesFromCell board player (Move startPosition)
    | insideTheBoard startPosition == False = []
    -- check for the empty cell
    | (playerAtCell board startPosition) /= (Nothing) = []
    | (availablePaths) == [] = []
    | otherwise = startPosition:availablePaths
    where
        availablePaths = concat (map (explore) (validDirections startPosition))
        explore :: Int -> [Int]
        explore direction = exploreNode (startPosition + direction) direction []
        -- we look for paths where we meet opponents cell(-s) followed by an empty cell in any of the directions
        exploreNode :: Int -> Int -> [Int] -> [Int]
        exploreNode currentPosition direction pathCells
            -- we do not include the source item, since it already belongs to a player, so we don't need to change it
            | playerCell = pathCells
            | opponentCell = exploreNode (currentPosition + direction) direction (currentPosition:pathCells)
            | otherwise = [] 
            where
                playerCell = isValidMove && ((playerAtCell board currentPosition) == (Just player))
                opponentCell = isValidMove && ((playerAtCell board currentPosition) == (Just (swapPlayer player)))
                -- we neeed to check if the value is valid and since we are going from the base to the empty, we need to recheck the positions
                -- so we don't get into 5->14 case
                isValidMove = (insideTheBoard currentPosition) && not leftEdgeDirection && not rightEdgeDirection
                leftEdgeDirection = currentPosition `mod` 8 == 0  && (direction == 9 || direction == (-7) || direction == 1)
                rightEdgeDirection = currentPosition `mod` 8 == 7  && (direction == 7 || direction == (-9) || direction == (-1))
        

-- selects the directions which we should check, we mostly need to avoid some moves on the right and left sight, since 5 -> 14 is not correct
validDirections :: Int -> [Int]
validDirections position | position `mod` 8 == 0  = [(-8), (-7), 1, 8, 9]
                         | position `mod` 8 == 7  = [(-9), (-8), (-1), 7, 8]
                         | otherwise = [(-9), (-8), (-7), (-1), 1, 7, 8, 9]
        
-- returns all the valid moves for the current player including the cell which will change alongside with it
validMoves :: Board -> Player -> [[Int]]
validMoves board player =  filter (\lst -> (null lst) == False) positions -- Return the resulting list as defined below
            where
  positions =  map (movesFromCell board player) [Move n | n <- [0..63]] 

checkDif [] = True
checkDif [x] = True
checkDif (x:y:xs) | y - x == 1 = checkDif (y:xs)
                  | otherwise = False

-- applying the move to the board
applyMove :: Board -> Maybe Player -> [Int] -> Board
applyMove board player [] = board
applyMove board player (x:moves) = applyMove board' player moves
                              where board' = updateCell board player x
                              
                              
-- return positions of the cell we are aiming to change 
target :: [Int] -> Move
target [] = Pass
target (x:xs) = Move x

-- return "random" move
atRandIndex :: [[Int]] -> [Int]  
atRandIndex [] = []
atRandIndex lst = (smallest lst)

randomMove :: Board -> Player -> Reversi.Move
randomMove board player = target(atRandIndex (validMoves board player))

-- selects the best move for the player
bestMove :: Board -> Player -> Reversi.Move
bestMove board player | powerSpots plays /= [] = target (powerSpots plays)
                      | applyLogic plays /= [] = target (smallest(applyLogic plays))
                      | otherwise = target (smallest plays)
                      where plays = validMoves board player

applyLogic :: [[Int]] -> [[Int]]
applyLogic moves = if null (logic) then moves else logic
                    where logic = (if pickMiddle moves == [] then pickCorner(moves) else pickMiddle(moves))

-- the strategy of playing the center of the field                    
pickMiddle :: [[Int]] -> [[Int]]
pickMiddle [] = []
pickMiddle (x:xs) = if (head x `mod` 8 )`elem` [2..5] && (head x `div` 8 )`elem` [2..5] then x:pickMiddle xs else pickMiddle xs

-- the strategy of trying to play more on corners
pickCorner :: [[Int]] -> [[Int]]
pickCorner [] = []
pickCorner (lst@(x:y:z):xs) = if abs (x`mod` 8 - y `mod` 8) >= 1 && abs (x `div` 8 - y `div` 8) >= 1 then lst:pickCorner xs else pickCorner xs

-- selects the smallest list 
smallest :: [[Int]] -> [Int]
smallest [] = []
smallest lst =minimumBy (comparing length) lst


--https://guides.net4tv.com/games/how-win-reversi
powerSpots :: [[Int]] -> [Int]
powerSpots [] = []
powerSpots (x:rest) | checkList x [0,7,56,63] = x
                    | checkList x [2,5,16,23,40,47,58,61] = x
                    | checkList x [18,21,42,45] = x
                    | otherwise = powerSpots rest

-- checks if the list has the elements of another 
checkList :: [Int] -> [Int] -> Bool
checkList [] _ = False
checkList (x:xs) lst = if x `elem` lst then True else checkList xs lst

-- shows only occupied cells
onlyOccupied :: Board -> Board
onlyOccupied [] = []
onlyOccupied ((Cell pos t):xs) | t == Nothing = onlyOccupied xs
                                  | otherwise = (Cell pos t) : (onlyOccupied xs)

-- simple printing function
printBoard :: Board -> IO ()
printBoard [] = return ()
printBoard ((Cell pos player):xs) 
                                  | player == Nothing = do putStr "--" 
                                                           printBoard xs
                                  | player == (Just Black) = do putStr "B" 
                                                                printBoard xs
                                  | player == (Just White) = do putStr "W" 
                                                                printBoard xs

