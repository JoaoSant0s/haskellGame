module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Geometry.Angle

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
  , reduceXVelocity:: Bool -- Reduce X velocity when respective direction have up button
  , reduceYVelocity:: Bool -- Reduce Y velocity when respective direction have up button
  , rotationLeft:: Bool
  , rotationRight:: Bool
  , rotation:: Float
  , savedRotation:: Float
  , ballDimension:: Float
  , directionX :: Float
  , directionY :: Float
  , reducingVelocity :: Float
  } deriving Show

  -- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (50, 50)  
  , reduceXVelocity = False
  , reduceYVelocity = False
  , rotationLeft = False
  , rotationRight = False
  , rotation = 0
  , savedRotation = 0
  , ballDimension = 10
  , directionY = 0
  , directionX = 0
  , reducingVelocity = 5
  }

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { directionY = dY', rotation = newRotation, savedRotation = sRotation, ballLoc = (x', y')}
  where
    -- Old locations and velocities.
    dX = directionX game
    dY = directionY game
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    newRotation = if rotationRight game
                  then (rotation game + 5)
                  else if rotationLeft game
                    then (rotation game - 5)
                    else rotation game

    sRotation = if not (reduceYVelocity game)
                    then newRotation
                    else savedRotation game

    rad = (degToRad (savedRotation game) )

    auxX = x + (sin rad) * vx * seconds * dY
    auxY = y + (cos rad) * vy * seconds * dY

    x' = if (auxX > (snd limitX) - (ballDimension game)) || (auxX < (fst limitX) + (ballDimension game))
         then x
         else auxX
         
    y' = if (auxY > (snd limitY) - (ballDimension game)) || (auxY < (fst limitY) + (ballDimension game))
         then y
         else auxY
    
    dY' = if reduceYVelocity game
          then (dY - seconds/(reducingVelocity game)) `max` 0
          else dY

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ rotate (rotation game) $ color ballColor $ Line [(0, 20), (15, -10), (8, -5), (-8, -5), (-15, -10), (0, 20)]
    ballColor = white

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
-- For an 's' keypress, reset the ball to the center.
-- handleKeys (EventKey (Char 'r') Down _ _) game = game { ballLoc = (0, 0) }

-- Ball control
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { directionX = -1, reduceXVelocity = False, rotationLeft = True}
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { reduceXVelocity = True, rotationLeft = False}

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { directionX = 1, reduceXVelocity = False, rotationRight = True}
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { reduceXVelocity = True, rotationRight = False }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { directionY = 1, reduceYVelocity = False}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { reduceYVelocity = True}
--handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { directionY = -1, reduceYVelocity = False}

--handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game { reduceYVelocity = True}

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
