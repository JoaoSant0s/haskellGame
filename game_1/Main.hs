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
  , ballVel :: Float  -- ^ Pong ball velocity.
  , ballVelAxis :: (Float, Float)
  , limitBallVel :: (Float, Float)
  , shipColor :: Color
  , attack :: Bool
  , bullets :: [Bullet]
  , incrementalVel :: Float
  , velocityIncrease :: Float
  , reduceXVelocity :: Bool -- Reduce X velocity when respective direction have up button
  , reduceYVelocity :: Bool -- Reduce Y velocity when respective direction have up button
  , rotationLeft :: Bool
  , rotationRight :: Bool
  , rotation :: Float
  , savedRotation :: Float
  , ballDimension :: Float
  , directionX :: Float
  , directionY :: Float
  , reducingVelocity :: Float
  } deriving Show

data Bullet = Bullet
  {bulletPosition :: (Float, Float)
  , bulletVelocity :: (Float, Float)
  , bullletColor :: Color
  } deriving Show
  -- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = 0
  , incrementalVel = 0
  , ballVelAxis = (0, 0)
  , shipColor = dark green
  , limitBallVel = (-200, 200)
  , bullets = []
  , attack = False
  , velocityIncrease = 0.1
  , reduceXVelocity = False
  , reduceYVelocity = True
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
moveBall seconds game = game { rotation = newRotation, ballVelAxis = (vX', vY'), incrementalVel = incrementalVel', savedRotation = sRotation, ballLoc = (x', y')}
  where
    -- Old locations and velocities.    
    (x, y) = ballLoc game    
    (vX, vY) = ballVelAxis game   
    incrementVel = incrementalVel game
    oldSavedRotation = savedRotation game
    oldRotation = rotation game
    v = sqrt (vX ** 2 + vY ** 2)
    
    incrementalVel' = if not (reduceYVelocity game)
                      then incrementVel + (velocityIncrease game)                                              
                      else 0

    newRotation = if rotationRight game
                  then (oldRotation + 5)
                  else if rotationLeft game
                    then (oldRotation - 5)
                    else oldRotation

    sRotation = if not (reduceYVelocity game)
                then newRotation
                else oldSavedRotation

    rad = (degToRad oldSavedRotation)

    auxVx = (vX + (sin rad) * incrementVel)
    auxVy = (vY + (cos rad) * incrementVel)
    
    vX' = auxVx

    vY' = auxVy

    auxX = x + vX * seconds
    auxY = y + vY * seconds    

    x' = if auxX > (snd limitX)
         then (fst limitX)
         else if auxX < (fst limitX)
          then (snd limitX)
          else auxX
         
    y' = if auxY > (snd limitY)       
         then (fst limitY)
         else if auxY < (fst limitY)
          then (snd limitY)
          else auxY  

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, renderBullets]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ rotate (rotation game) $ color (shipColor game) $ Line [(0, 20), (12, -10), (5, -5), (-5, -5), (-12, -10), (0, 20)]
    ballColor = dark green

    renderBullets = createBullets (bullets game)

    createBullet :: Bullet -> Picture
    createBullet b = translate (fst (bulletPosition b)) (snd (bulletPosition b)) $ color white $ Circle 10

    createBullets :: [Bullet] -> Picture
    createBullets bs = pictures (map createBullet bs)    
    --bulletObject = Bullet (ballLoc game) (0, 0) white 

    {- bullet = if attack game
             then Circle 10
             else Blank -} 

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- Ball control
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { directionX = -1, reduceXVelocity = False, rotationLeft = True}
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { reduceXVelocity = True, rotationLeft = False}

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { directionX = 1, reduceXVelocity = False, rotationRight = True}
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { reduceXVelocity = True, rotationRight = False }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { reduceYVelocity = False}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { incrementalVel = 0, reduceYVelocity = True}

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = game {attack = True}

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
update seconds = moveShot . (createShot . moveBall seconds)

createShot :: PongGame -> PongGame
createShot game = game {bullets = bullets', shipColor = color, attack = atk}
  where
    oldBullets = bullets game
    attacking = attack game
    atk = False

    color = if attacking
            then dark red
            else dark green

    bullets' = oldBullets ++ bullet

    bullet = if attacking
            then [(Bullet (ballLoc game) (0, 0) white)]
            else []
moveShot :: PongGame -> PongGame
moveShot game = game