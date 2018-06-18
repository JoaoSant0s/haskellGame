module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

width, height, offset, offsetWidth :: Int
offsetWidth = 15
paddleWidth = 26
paddleHeight = 86
width = 800
height = 600
offset = 100

limitX :: (Float, Float)
limitX = ( - fromIntegral width / 2, fromIntegral width / 2 )

limitY :: (Float, Float)
limitY = ( - fromIntegral height / 2, fromIntegral height / 2 )

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  {- , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Right player paddle height. -}
  , reduceXVelocity:: Bool -- Reduce X velocity when respective direction have up button
  , reduceYVelocity:: Bool -- Reduce Y velocity when respective direction have up button
  , ballDimension:: Float
  , directionX :: Float
  , directionY :: Float
  , reducingVelocity :: Float
  } deriving Show

  -- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (20, 20)
  {- , player1 = 40
  , player2 = 40 -}
  , reduceXVelocity = False
  , reduceYVelocity = False
  , ballDimension = 10
  , directionY = 0
  , directionX = 0
  , reducingVelocity = 5
  }

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y'), directionX = dX', directionY = dY'}
  where
    -- Old locations and velocities.
    dX = directionX game
    dY = directionY game
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    auxX = x + vx * seconds * dX
    auxY = y + vy * seconds * dY

    x' = if (auxX > (snd limitX) - (ballDimension game)) || (auxX < (fst limitX) + (ballDimension game))
         then x
         else auxX
         
    y' = if (auxY > (snd limitY) - (ballDimension game)) || (auxY < (fst limitY) + (ballDimension game))
         then y
         else auxY

    dX' = if not (reduceXVelocity game)
          then dX
          else if dX < 0
            then (dX + seconds/(reducingVelocity game)) `min` 0
            else (dX - seconds/(reducingVelocity game)) `max` 0

    dY' = if not (reduceYVelocity game)
          then dY
          else if dY < 0
            then (dY + seconds/(reducingVelocity game)) `min` 0
            else (dY - seconds/(reducingVelocity game)) `max` 0

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball{- , walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game -}]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid (ballDimension game)
    ballColor = dark red

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
-- For an 's' keypress, reset the ball to the center.
-- handleKeys (EventKey (Char 'r') Down _ _) game = game { ballLoc = (0, 0) }

-- Ball control
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { directionX = -1, reduceXVelocity = False}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { directionX = 1, reduceXVelocity = False }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { reduceXVelocity = True}
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { reduceXVelocity = True }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { directionY = 1, reduceYVelocity = False}
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { directionY = -1, reduceYVelocity = False}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { reduceYVelocity = True}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game { reduceYVelocity = True}

-- Do nothing for all other events.
handleKeys _ game = game

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> PongGame -> PongGame
update seconds = moveBall seconds