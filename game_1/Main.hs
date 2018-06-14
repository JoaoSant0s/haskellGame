module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

width, height, offset, offsetWidth :: Int
offsetWidth = 15
paddleWidth = 26
paddleHeight = 86
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Right player paddle height.
  } deriving Show

  -- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (10, -30)
  , player1 = 40
  , player2 = 40
  }

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

type Radius = Float 
type Position = (Float, Float)

paddleCollision :: Position -> Radius -> Float -> Float -> Bool 
paddleCollision (x, y) radius player1Y player2Y =  player1Extremities || player2Extremities
  where
    -- Right (Player 1) Collision condition
    player1Extremities = rightCollisionX && rightCollisionY
    rightCollisionX = rightPositionX >=  rightLimitX  
    rightLimitX = fromIntegral width / 2    
    rightPositionX = x + radius + (paddleWidth + fromIntegral offsetWidth)

    rightCollisionY = (rightLimitYMin <= y) && (y <= rightLimitYMax)
    rightLimitYMax = player1Y + (paddleHeight / 2)
    rightLimitYMin = player1Y - (paddleHeight / 2)    
    -- player1Y
    -- paddleHeight

    -- Left (Player 2) Collision condition
    player2Extremities = leftCollision && leftCollisionY
    leftCollision = leftPosition <=  leftLimitX
    leftLimitX = -fromIntegral width / 2
    leftPosition = x - radius + (-paddleWidth - fromIntegral offsetWidth)

    leftCollisionY = (leftLimitYMin <= y) && (y <= leftLimitYMax)
    leftLimitYMax = player2Y + (paddleHeight / 2)
    leftLimitYMin = player2Y - (paddleHeight / 2)  
          
    -- player1Y
    -- paddleHeight

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    -- Top Collision condition
    topCollision = topPosition <= topLimit
    topLimit = -fromIntegral width / 2
    topPosition = y - radius - 5

    -- Bottom Collision condition
    bottomCollision = y + radius + 5 >=  fromIntegral width / 2
    bottomLimit = fromIntegral width / 2
    bottomPosition = y + radius + 5

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vx' = if paddleCollision (ballLoc game) radius (player1 game) (player2 game)
          then
             -- Update the velocity.
             -vx
           else
            -- Do nothing. Return the old velocity.
            vx

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'r') Down _ _) game = game { ballLoc = (0, 0) }
-- Player 2 Up and Down
handleKeys (EventKey (Char 'w') Down _ _) game = game { player2 = ((player2 game) + 5) }
handleKeys (EventKey (Char 's') Down _ _) game = game { player2 = ((player2 game) - 5) }
-- Player 1 Up and DOwn
handleKeys (EventKey (Char 'i') Down _ _) game = game { player1 = ((player1 game) + 5) }
handleKeys (EventKey (Char 'k') Down _ _) game = game { player1 = ((player1 game) - 5) }
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
update seconds = paddleBounce . (wallBounce . moveBall seconds)