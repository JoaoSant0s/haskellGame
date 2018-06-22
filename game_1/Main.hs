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

shipDraw :: Path
shipDraw = [(0, 20), (12, -10), (5, -5), (-5, -5), (-12, -10), (0, 20)]

window :: Display
window = InWindow "Asteroids" (width, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the pong game.
data PongGame = Game
  { shipLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.  
  , shipVelAxis :: (Float, Float)  
  , shipColor :: Color  
  , attack :: Bool  
  , bullets :: [Bullet]
  , asteroids :: [Asteroid] 
  , incrementalVel :: Float
  , velocityIncrease :: Float
  , reduceYVelocity :: Bool
  , rotationLeft :: Bool
  , rotationRight :: Bool
  , rotation :: Float
  , savedRotation :: Float
  , distanceBullet :: Float
  } deriving Show

data Bullet = Bullet
  { bulletPosition :: (Float, Float)
  , bulletVelocity :: (Float, Float)
  , bulletColor :: Color
  , bulletSize :: Float
  , maxTime :: Float
  , time :: Float
  } deriving Show

data Asteroid = Asteroid
  { asteroidPosition  :: (Float, Float)        
  , asteroidVelocity  :: (Float, Float)        
  , asteroidColor :: Color
  , asteroidSize :: Int        
  } deriving Show

  -- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { shipLoc = (0, 0)  
  , incrementalVel = 0
  , shipVelAxis = (0, 0)
  , shipColor = dark green  
  , bullets = []
  , asteroids = initAsteroids
  , attack = False  
  , velocityIncrease = 0.1
  , reduceYVelocity = True
  , rotationLeft = False
  , rotationRight = False
  , rotation = 0
  , savedRotation = 0
  , distanceBullet = 25
  }

initAsteroids :: [Asteroid]
initAsteroids = [Asteroid {asteroidPosition = (150, 150), asteroidVelocity = (-10, -10), asteroidColor = white, asteroidSize = 4}]

asteroidDimension :: Int -> Path
asteroidDimension index 
  | index == 4 = [(50, 50), (50, -50), (-50, -50), (-50, 50), (50, 50)]

moveAsteroids :: Float -> PongGame -> PongGame
moveAsteroids seconds game = game { asteroids = asteroids' }
  where
    asteroids' = map moveAsteroid (asteroids game)    
    
    moveAsteroid :: Asteroid -> Asteroid
    moveAsteroid b = b {asteroidPosition = ( x', y')}
      where        
        x' = updateAxisPosition auxX limitX
        y' = updateAxisPosition auxY limitY

        (x, y) = asteroidPosition b
        (vX, vY) = (asteroidVelocity b)
        auxX = x + vX * seconds
        auxY = y + vY * seconds

createBullet :: PongGame -> PongGame
createBullet game = game {bullets = bullets', attack = attack'}
  where
    oldBullets = bullets game
    attacking = attack game
    bulletDistance = distanceBullet game

    bullet = if attacking
             then bulletObject
             else []
    
    bulletObject = [Bullet { bulletPosition = newPosition, bulletVelocity = bulletVelocity, bulletColor = (dark red), bulletSize =  2.5, maxTime = 4, time = 0}] 
    newPosition = axisPosition (shipLoc game) rad    
    bulletVelocity = (200 * (sin rad), 200 * (cos rad))

    rad = degToRad (rotation game)        
    attack' = False    

    bullets' = oldBullets ++ bullet
    
    axisPosition :: (Float, Float) -> Float -> (Float, Float)
    axisPosition (x, y) rad = (x + (sin rad) * bulletDistance, y + (cos rad) * bulletDistance)

moveBullets :: Float -> PongGame -> PongGame
moveBullets seconds game =  game { bullets = bullets' }
  where
    bullets' = map moveBullet filteredBullets
              
    filteredBullets = filter (\b -> (time b) < (maxTime b) ) currentBullets
    currentBullets = (bullets game)
    
    moveBullet :: Bullet -> Bullet
    moveBullet b = b {bulletPosition = ( x', y'), time = time'}
      where
        time' = (time b) + seconds
        x' = updateAxisPosition auxX limitX
        y' = updateAxisPosition auxY limitY

        (x, y) = bulletPosition b
        (vX, vY) = (bulletVelocity b)
        auxX = x + vX * seconds
        auxY = y + vY * seconds        
    
-- | Update the ball position using its current velocity.
moveShip :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveShip seconds game = game { rotation = newRotation, shipVelAxis = (vX', vY'), incrementalVel = incrementalVel', savedRotation = sRotation, shipLoc = (x', y')}
  where
    -- Old locations and velocities.    
    (x, y) = shipLoc game    
    (vX, vY) = shipVelAxis game   
    incrementVel = incrementalVel game
    oldSavedRotation = savedRotation game
    oldRotation = rotation game  
    
    incrementalVel' = if not (reduceYVelocity game) then incrementVel + (velocityIncrease game) else incrementVel

    newRotation = if rotationRight game
                  then (oldRotation + 5)
                  else if rotationLeft game
                    then (oldRotation - 5)
                    else oldRotation

    sRotation = if not (reduceYVelocity game) then newRotation else oldSavedRotation

    rad = (degToRad oldSavedRotation)    
    
    vX' =  (vX + (sin rad) * incrementVel)
    vY' = (vY + (cos rad) * incrementVel)

    auxX = x + vX * seconds
    auxY = y + vY * seconds    

    x' = updateAxisPosition auxX limitX
    y' = updateAxisPosition auxY limitY

updateAxisPosition :: Float -> (Float, Float) -> Float
updateAxisPosition nextValue (minLimit, maxLimit) 
  | nextValue > maxLimit = minLimit
  | nextValue < minLimit = maxLimit
  | otherwise = nextValue

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, renderBullets, renderAsteroids]
  where
    --  The pong ball.
    ball = uncurry translate (shipLoc game) $ rotate (rotation game) $ color (shipColor game) $ Line shipDraw
    ballColor = dark green

    renderBullets = drawBullets (bullets game)
    renderAsteroids = drawAsteroids (asteroids game)

    drawBullet :: Bullet -> Picture
    drawBullet b = translate (fst (bulletPosition b)) (snd (bulletPosition b)) $ color (bulletColor b) $ circleSolid (bulletSize b)
    
    drawBullets :: [Bullet] -> Picture
    drawBullets bs = pictures (map drawBullet bs)

    drawAsteroid :: Asteroid -> Picture
    drawAsteroid a = translate (fst (asteroidPosition a)) (snd (asteroidPosition a)) $ color (asteroidColor a) $ (Line (asteroidDimension (asteroidSize a) ) )

    drawAsteroids :: [Asteroid] -> Picture
    drawAsteroids as = pictures (map drawAsteroid as)


    --bulletObject = Bullet (shipLoc game) (0, 0) white    

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- Ball control
handleKeys (EventKey (SpecialKey KeyCtrlL) Down _ _) game = game {attack = True}

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { rotationLeft = True}
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { rotationLeft = False}

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { rotationRight = True}
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { rotationRight = False }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { reduceYVelocity = False}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { incrementalVel = 0, reduceYVelocity = True}

handleKeys _ game = game

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> PongGame -> PongGame
update seconds = moveAsteroids seconds . (moveBullets seconds . (createBullet . moveShip seconds))
