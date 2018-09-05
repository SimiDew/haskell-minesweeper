module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Data.Array

--import Game
--import Logic
--import Rendering

data CellState = Revealed | Flagged | Mine | Unrevealed deriving (Eq, Show)
type Cell = Maybe CellState
data State = Running | GameOver (Maybe MinesFound) deriving (Eq, Show)
data MinesFound = Found | Died deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gameState :: State
                 , gameCellState :: CellState
                 , gameMinesFound :: MinesFound
                 }

n :: Int
n = 25

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gameState = Running
                   , gameCellState = Unrevealed -- | add MinesFound
                   , gameMinesFound = Found
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))

boardAsRunningPicture board =
    pictures [ color boardGridColor $ boardGrid ]

boardGridColor = makeColorI 0 0 0 255
foundColor = makeColorI 255 50 50 255
diedColor = makeColorI 50 100 255 255

outcomeColor (Just Found) = foundColor
outcomeColor (Just Died) = diedColor

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

unrevealedCell :: Picture
unrevealedCell = Blank

revealedCell :: Picture
revealedCell = Blank

flaggedCell :: Picture
flaggedCell = Blank

mineCell :: Picture
mineCell = Blank

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

unrevealedCellsOfBoard :: Board -> Picture
unrevealedCellsOfBoard board = cellsOfBoard board (Just Unrevealed) unrevealedCell

revealedCellsOfBoard :: Board -> Picture
revealedCellsOfBoard board = cellsOfBoard board (Just Revealed) revealedCell

flaggedCellsOfBoard :: Board -> Picture
flaggedCellsOfBoard board = cellsOfBoard board (Just Flagged) flaggedCell

mineCellOfBoard :: Board -> Picture
mineCellOfBoard board = cellsOfBoard board (Just Mine) mineCell

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

boardAsPicture board =
    pictures [ unrevealedCellsOfBoard board
             , revealedCellsOfBoard board
             , flaggedCellsOfBoard board
             , mineCellOfBoard board
             , boardGrid]

--outcomeColor (Just

boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)

gameAsPicture :: Game -> Picture
gameAsPicture game =
    case gameState game of
        Running -> boardAsRunningPicture (gameBoard game)
        GameOver winner -> boardAsGameOverPicture winner (gameBoard game)

-- Game

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

window = InWindow "Minesweeper" (screenWidth, screenHeight) (100, 100)
background = makeColor 255 255 255 255
fps = 30
updateGame _ game = game

main :: IO ()
main = play window background fps initialGame gameAsPicture updateGame (const id)