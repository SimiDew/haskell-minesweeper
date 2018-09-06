module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Data.Array

import Graphics.Gloss.Interface.Pure.Game

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

initialGame = Game { gameBoard = (array indexRange $ zip (range indexRange) (repeat (Just Unrevealed))) // [((1, 3), Just Revealed)]
                   , gameState = Running
                   , gameCellState = Unrevealed -- | add MinesFound
                   , gameMinesFound = Found
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))

boardAsRunningPicture board =
    pictures [ color boardGridColor $ boardGrid
             , color unrevealedColor $ unrevealedCellsOfBoard board
             , color revealedColor $ revealedCellsOfBoard board
             ]

boardGridColor = makeColorI 0 0 0 255
foundColor = makeColorI 50 100 255 255
diedColor = makeColorI 255 50 50 255
unrevealedColor = makeColorI 173 173 173 255
revealedColor = makeColorI 221 221 221 255

outcomeColor (Just Found) = foundColor
outcomeColor (Just Died) = diedColor
outcomeColor Nothing = greyN 0.5

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

unrevealedCell :: Picture
unrevealedCell = rectangleSolid side 24.0
    where side = min cellWidth cellHeight * 0.95

revealedCell :: Picture
revealedCell = rectangleSolid side 24.0
    where side = min cellWidth cellHeight * 0.95

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

boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture (gameBoard game)
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game)

-- Logic

isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && board ! cellCoord == Just Unrevealed =
        game { gameBoard = board // [(cellCoord, Just Revealed)] }
    | otherwise = game
    where board = gameBoard game



mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

updateGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
updateGame _ game = game

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
background = makeColor 221 221 221 255
fps = 30

main :: IO ()
main = play window background fps initialGame gameAsPicture updateGame (const id)