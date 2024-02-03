{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Graphics.Vty (Event (EvKey), Key (KChar, KDown, KLeft, KRight, KUp), defAttr, green, red, white)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Random

data V2 a = V2 a a deriving (Show, Eq)

instance (Num a) => Num (V2 a) where
    (+) (V2 x y) (V2 z w) = V2 (x + z) (y + w)
    (-) (V2 x y) (V2 z w) = V2 (x - z) (y - w)
    (*) (V2 x y) (V2 z w) = V2 (x * z) (y * w)
    abs (V2 x y) = V2 (abs x) (abs y)
    signum (V2 x y) = V2 (signum x) (signum y)
    fromInteger x = V2 (fromInteger x) (fromInteger x)

gridSize :: V2 Int
gridSize = V2 20 20

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y

data Direction = North | East | South | West deriving (Show, Eq)
type Coord = V2 Int
type Snake = [Coord]

data Game = Game
    { _snake :: Snake
    , _food :: Coord
    , _nextFood :: [Coord]
    , _dir :: Main.Direction
    , _score :: Int
    , _dead :: Bool
    }
    deriving (Show, Eq)

makeLenses ''Game

data Tick = Tick
type Name = ()

data Cell = Empty | Snake | Food deriving (Show, Eq)

drawUI :: Game -> [Widget Name]
drawUI st = [center $ scoreBox <+> grid]
  where
    scoreBox = borderWithLabel (str " Score ") . withBorderStyle unicode . padLeftRight 5 . padTopBottom 1 . str . show $ st ^. score
    grid = borderWithLabel (str " Snake ") $ withBorderStyle unicode rows
    rows =
        vBox
            [ hBox
                [ drawCell $ V2 x y | x <- [0 .. getX gridSize]
                ]
            | y <- [0 .. getY gridSize]
            ]
    drawCell x
        | x `elem` st ^. snake = withAttr snakeCellAttr $ str "  "
        | x == st ^. food = withAttr foodCellAttr $ str "  "
        | otherwise = withAttr emptyCellAttr $ str "  "

handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (AppEvent Tick) = modify step
handleEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
handleEvent (VtyEvent (EvKey (KChar 'r') [])) = do
    newState <- liftIO initialState
    modify $ const newState
handleEvent (VtyEvent (EvKey KUp [])) = modify $ turn North
handleEvent (VtyEvent (EvKey KRight [])) = modify $ turn East
handleEvent (VtyEvent (EvKey KDown [])) = modify $ turn South
handleEvent (VtyEvent (EvKey KLeft [])) = modify $ turn West
handleEvent _ = return ()

step :: Game -> Game
step st = if st ^. dead then st else move st

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [_] = []
removeLast (x : xs) = x : removeLast xs

removeFirst :: [a] -> [a]
removeFirst [] = []
removeFirst (_ : xs) = xs

move :: Game -> Game
move st = stateAfterEating
  where
    theSnake = st ^. snake
    snakeHead = head theSnake
    newSnakeHead = case st ^. dir of
        North -> snakeHead + V2 0 (-1)
        East -> snakeHead + V2 1 0
        South -> snakeHead + V2 0 1
        West -> snakeHead + V2 (-1) 0
    newSnake = loopSnakeHead newSnakeHead : removeLast theSnake
    isDead = newSnakeHead `elem` tail newSnake
    stateAfterMove =
        if isDead
            then st & dead .~ True
            else st & snake .~ newSnake
    isEatingFood = newSnakeHead == st ^. food
    stateAfterEating =
        if isEatingFood
            then eatFood stateAfterMove
            else stateAfterMove

eatFood :: Game -> Game
eatFood st = st & score +~ 1 & food .~ head (st ^. nextFood) & nextFood %~ removeFirst & snake %~ addSnakeSegment

turn :: Main.Direction -> Game -> Game
turn North st = if st ^. dir == South then st else st & dir .~ North
turn East st = if st ^. dir == West then st else st & dir .~ East
turn South st = if st ^. dir == North then st else st & dir .~ South
turn West st = if st ^. dir == East then st else st & dir .~ West

loopSnakeHead :: Coord -> Coord
loopSnakeHead (V2 x y) = V2 (x `mod` (getX gridSize + 1)) (y `mod` (getY gridSize + 1))

addSnakeSegment :: Snake -> Snake
addSnakeSegment s = s ++ [head s]

emptyCellAttr, snakeCellAttr, foodCellAttr :: AttrName
emptyCellAttr = attrName "emptyCell"
snakeCellAttr = attrName "snakeCell"
foodCellAttr = attrName "foodCell"

theMap :: AttrMap
theMap =
    attrMap
        defAttr
        [ (emptyCellAttr, bg white)
        , (snakeCellAttr, bg green)
        , (foodCellAttr, bg red)
        ]

app :: App Game Tick Name
app =
    App
        { appStartEvent = return ()
        , appHandleEvent = handleEvent
        , appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appAttrMap = const theMap
        }

initialState :: IO Game
initialState = do
    xValues <- mapM (const $ randomRIO (1, getX gridSize)) [1 .. getX (gridSize * gridSize)]
    yValues <- mapM (const $ randomRIO (1, getY gridSize)) [1 .. getY (gridSize * gridSize)]
    return $
        Game
            { _snake =
                [ V2 (getX gridSize `div` 2) (getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 1 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 2 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 3 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 4 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 5 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 6 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 7 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 8 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 9 $ getY gridSize `div` 2)
                -- , V2 (getX gridSize `div` 2) ((+) 10 $ getY gridSize `div` 2)
                ]
            , _food = V2 1 1
            , _dir = North
            , _score = 0
            , _dead = False
            , _nextFood = zipWith V2 xValues yValues
            }

main :: IO ()
main = do
    chan <- newBChan 10
    void $ forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100000
    st <- initialState
    void $ customMainWithDefaultVty (Just chan) app st
