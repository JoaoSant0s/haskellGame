module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector

width, height, offsetWidth, offsetHeight :: Int
width = 1024
height = 650
offsetWidth = 150
offsetHeight = 15

limitX :: (Float, Float)
limitX = ( - fromIntegral width / 2, fromIntegral width / 2 )

limitY :: (Float, Float)
limitY = ( - fromIntegral height / 2, fromIntegral height / 2 )

shipDraw :: Path
shipDraw = [(0, 20), (12, -10), (5, -5), (-5, -5), (-12, -10), (0, 20)]

window :: Display
window = InWindow "Asteroids" (width, height) (offsetWidth, offsetHeight)

background :: Color
background = black

-- | Data describing the state of the asteroids game.
data AsteroidsGame = Game
  { shipLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.  
  , shipVelAxis :: (Float, Float)  
  , shipDimension :: Float
  , shipColor :: Color 
  , attackTime :: Float
  , attackDelay :: Float
  , attack :: Bool  
  , bullets :: [Bullet]
  , asteroids :: [Asteroid] 
  , incrementalVel :: Float
  , maxIncrementalVel :: Float
  , velocityIncrease :: Float
  , acelerate :: Bool
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
  , asteroidSize :: Float
  , asteroidVelocityRotation :: Float
  , asteroidRotation :: Float
  } deriving Show

initialState :: AsteroidsGame
initialState = Game
  { shipLoc = (0, 0)  
  , shipDimension = 10
  , incrementalVel = 0
  , maxIncrementalVel = 10
  , shipVelAxis = (0, 0)
  , shipColor = dark green  
  , bullets = []
  , asteroids = initAsteroids
  , attackTime = -1
  , attackDelay = 0.25
  , attack = False  
  , velocityIncrease = 0.1
  , acelerate = False
  , rotationLeft = False
  , rotationRight = False
  , rotation = 0
  , savedRotation = 0
  , distanceBullet = 25
  }

resetState :: AsteroidsGame -> AsteroidsGame
resetState game = game 
  { shipLoc = (0, 0)  
  , incrementalVel = 0
  , shipVelAxis = (0, 0)  
  , bullets = []
  , asteroids = initAsteroids
  , attack = False  
  , velocityIncrease = 0.1
  , acelerate = False
  , rotationLeft = False
  , rotationRight = False
  , rotation = 0
  , savedRotation = 0  
  }

initAsteroids :: [Asteroid]
initAsteroids = asteroids
  where
    asteroids = [
      Asteroid {asteroidPosition = (600, 150), asteroidVelocity = (-20, -10), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 0.75, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-350, 150), asteroidVelocity = (-10, -20), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -0.5, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-400, -150), asteroidVelocity = (-20, -20), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (600, -250), asteroidVelocity = (20, -30), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -0.75, asteroidRotation = 0},
      Asteroid {asteroidPosition = (50, 100), asteroidVelocity = (10, -30), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-600, 60), asteroidVelocity = (-20, 20), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -0.25, asteroidRotation = 0},
      Asteroid {asteroidPosition = (350, -150), asteroidVelocity = (-20, -10), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-200, -250), asteroidVelocity = (-30, -10), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (500, -50), asteroidVelocity = (-20, -10), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 0.15, asteroidRotation = 0},
      Asteroid {asteroidPosition = (50, 100), asteroidVelocity = (20, -30), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (100, -100), asteroidVelocity = (10, 20), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 0.5, asteroidRotation = 0},

      Asteroid {asteroidPosition = (300, -300), asteroidVelocity = (-10, -30), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (350, 20), asteroidVelocity = (20, 20), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-200, -50), asteroidVelocity = (-10, -10), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (0430, -50), asteroidVelocity = (20, -30), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-50, 100), asteroidVelocity = (30, -20), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-60, 100), asteroidVelocity = (-20, 20), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 1, asteroidRotation = 0},
      Asteroid {asteroidPosition = (30, -350), asteroidVelocity = (-20, -30), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -0.25, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-120, 150), asteroidVelocity = (5, -10), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 0.5, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-50, 200), asteroidVelocity = (10, -25), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = -0.5, asteroidRotation = 0},
      Asteroid {asteroidPosition = (-50, -50), asteroidVelocity = (-5, -20), asteroidColor = white, asteroidSize = 10, asteroidVelocityRotation = 1, asteroidRotation = 0}
      ]

asteroidDimension :: Float -> Path
asteroidDimension size
  | size == 10 = [(5, 10), (10, 5), (10, -7), (7, -10), (-7, -7), (-10, 2), (-5, 5), (-2, 10), (5, 10)]

moveAsteroids :: Float -> AsteroidsGame -> AsteroidsGame
moveAsteroids seconds game = game { asteroids = asteroids' }
  where
    asteroids' = map moveAsteroid (asteroids game)    
    
    moveAsteroid :: Asteroid -> Asteroid
    moveAsteroid b = b {asteroidPosition = ( x', y'), asteroidRotation = asteroidRotation'}
      where        
        x' = updateAxisPosition auxX limitX
        y' = updateAxisPosition auxY limitY
        asteroidRotation' = (asteroidRotation b) + (asteroidVelocityRotation b)

        (x, y) = asteroidPosition b
        (vX, vY) = (asteroidVelocity b)
        auxX = x + vX * seconds
        auxY = y + vY * seconds

createBullet :: Float -> AsteroidsGame -> AsteroidsGame
createBullet seconds game = game {bullets = bullets', attack = attack', attackTime = attackTime'}
  where
    oldBullets = bullets game
    attacking = attack game
    bulletDistance = distanceBullet game

    attackT = attackTime game
    attackD = attackDelay game    

    (attackTime', bullet) = if attackT < 0 && attacking then (attackD, bulletObject) else (attackT - seconds, [])
    
    bulletObject = [Bullet { bulletPosition = newPosition, bulletVelocity = bulletVelocity, bulletColor = (dark red), bulletSize =  2.5, maxTime = 4, time = 0}] 
    newPosition = axisPosition (shipLoc game) rad    
    bulletVelocity = (200 * (sin rad), 200 * (cos rad))

    rad = degToRad (rotation game)        
    attack' = False    

    bullets' = oldBullets ++ bullet
    
    axisPosition :: (Float, Float) -> Float -> (Float, Float)
    axisPosition (x, y) rad = (x + (sin rad) * bulletDistance, y + (cos rad) * bulletDistance)

moveBullets :: Float -> AsteroidsGame -> AsteroidsGame
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

moveShip :: Float -> AsteroidsGame -> AsteroidsGame
moveShip seconds game = game { rotation = newRotation, shipVelAxis = (vX', vY'), incrementalVel = incrementalVel', savedRotation = sRotation, shipLoc = (x', y')}
  where
    -- Old locations and velocities.    
    (x, y) = shipLoc game    
    (vX, vY) = shipVelAxis game   
    incrementVel = incrementalVel game
    maxIncrementVel = maxIncrementalVel game
    oldSavedRotation = savedRotation game
    oldRotation = rotation game  
    
    incrementalVel' = if acelerate game && not ( (rotationRight game) || (rotationLeft game) )
                      then incrementVel + (velocityIncrease game) 
                      else if (rotationRight game) || (rotationLeft game)
                        then 0 
                        else incrementVel
    --local = if acelerate game && incrementVel < maxIncrementVel then incrementVel + (velocityIncrease game) else 0

    newRotation = if rotationRight game
                  then (oldRotation + 5)
                  else if rotationLeft game
                    then (oldRotation - 5)
                    else oldRotation

    sRotation = if acelerate game then newRotation else oldSavedRotation

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

checkCollision :: AsteroidsGame -> AsteroidsGame
checkCollision game = if reset then newGame else game {bullets = bullets', asteroids = asteroids'}
  where
    (newGame, reset) = checkShipAsteroids game  
    (bullets', asteroids') = checkBulletAsteroids (bullets game , asteroids game)     

checkShipAsteroids :: AsteroidsGame -> (AsteroidsGame, Bool)
checkShipAsteroids game = if length collidedShip > 0 then (resetState game, True) else (game, False)
  where        
    collidedShip = filter (\asteroid -> (collideAsteroidShip asteroid (shipLoc game) (shipDimension game) ) ) (asteroids game)

    collideAsteroidShip:: Asteroid -> (Float, Float) -> Float -> Bool
    collideAsteroidShip a shipPosition shiSize = collides shipPosition shiSize (asteroidPosition a) (asteroidSize a)
    
checkBulletAsteroids :: ([Bullet], [Asteroid]) -> ([Bullet], [Asteroid])
checkBulletAsteroids (bullets, asteroids) = (newBullets, newAsteroids)
  where
    newAsteroids = filterAsteroids bullets asteroids
    newBullets = filterBullets bullets asteroids        

    filterAsteroids :: [Bullet] -> [Asteroid] -> [Asteroid]
    filterAsteroids (bullet:bullets) asteroids = filterAsteroids bullets (notCollidedAsteroids asteroids bullet)
    filterAsteroids bullets asteroids = asteroids

    filterBullets :: [Bullet] -> [Asteroid] -> [Bullet]
    filterBullets bullets (asteroid : asteroids) = filterBullets (notCollidedBullets bullets asteroid) asteroids     
    filterBullets bullets asteroids = bullets

    notCollidedAsteroids :: [Asteroid] -> Bullet -> [Asteroid]
    notCollidedAsteroids asteroids bullet  = filter (\asteroid -> not (collideAsteroidBullet asteroid bullet ) ) asteroids

    notCollidedBullets :: [Bullet] -> Asteroid -> [Bullet]
    notCollidedBullets bullets asteroid = filter (\bullet -> not (collideAsteroidBullet asteroid bullet ) ) bullets

    collideAsteroidBullet:: Asteroid -> Bullet -> Bool
    collideAsteroidBullet a b = collides (bulletPosition b) (bulletSize b) (asteroidPosition a) (asteroidSize a)

collides :: Point -> Float -> Point -> Float -> Bool
collides (x1, y1) s1 (x2, y2) s2 = magV (subtraction) < s1 + s2
  where
    subtraction = (x1 - x2, y1 - y2)

render :: AsteroidsGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game = pictures [ship, renderBullets, renderAsteroids]
  where    
    ship = uncurry translate (shipLoc game) $ rotate (rotation game) $ color (shipColor game) $ Line shipDraw    

    renderBullets = drawBullets (bullets game)
    renderAsteroids = drawAsteroids (asteroids game)

    drawBullet :: Bullet -> Picture
    drawBullet b = uncurry translate (bulletPosition b) $ color (bulletColor b) $ circleSolid (bulletSize b)
    
    drawBullets :: [Bullet] -> Picture
    drawBullets bs = pictures (map drawBullet bs)

    drawAsteroid :: Asteroid -> Picture
    drawAsteroid a = uncurry translate (asteroidPosition a) $ rotate (asteroidRotation a) $ color (asteroidColor a) $ Line (asteroidDimension (asteroidSize a ) )

    drawAsteroids :: [Asteroid] -> Picture
    drawAsteroids as = pictures (map drawAsteroid as)     

-- | Respond to key events.
handleKeys :: Event -> AsteroidsGame -> AsteroidsGame

-- Ball control
handleKeys (EventKey (SpecialKey KeyCtrlL) Down _ _) game = game {attack = True}

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { rotationLeft = True}
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { rotationLeft = False}

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { rotationRight = True}
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { rotationRight = False }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { acelerate = True}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { incrementalVel = 0, acelerate = False}

handleKeys (EventKey (Char 'r') Down _ _) game = resetState game

handleKeys _ game = game

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> AsteroidsGame -> AsteroidsGame
update seconds = checkCollision . (moveAsteroids seconds . (moveBullets seconds . (createBullet seconds . (moveShip seconds) ) ) )

