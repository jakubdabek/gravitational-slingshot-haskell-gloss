{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
module Main(main) where

import System.Environment (getArgs)
import Numeric (showEFloat)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Data.Picture (Path)
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import qualified Graphics.Gloss.Data.Vector as Vec

width, height, offset :: Int
width  = 1000
height = 600
offset = 100

window :: Display
window = InWindow "A Window" (width, height) (offset, offset)

background :: Color
background = black

type Position = Vec.Point
type Mass     = Float
type Velocity = Vec.Vector

gravitationalConstant :: Float
gravitationalConstant = 6.67e-11


data Object = Object
    { mass     :: Mass
    , position :: Position
    , velocity :: Velocity
    }

data SimulationState = Simulation { objects :: [Object], trace :: Path, lastTraceUpdate :: Time }

planet :: SimulationState -> Object
planet = (!! 0) . objects
moon :: SimulationState -> Object
moon = (!! 1) . objects
ship :: SimulationState -> Object
ship = (!! 2) . objects

acceleratingAngle, deceleratingAngle :: Float
acceleratingAngle = 0.56
deceleratingAngle = 0.565

initialState :: Float -> SimulationState
initialState angle = Simulation
    [ Object
        { mass = planetMass
        , position = (0, 0)
        , velocity = (0, 0)
        }
    , Object
        { mass = moonMass
        , position = (orbitRadius, 0)
        , velocity = moonVelocity
        }
    , Object
        { mass = shipMass
        , position = shipInitialPosition
        , velocity = shipVelocity
        }
    ]
    []
    0
    where planetMass = 5.972e24
          moonMass = 7.348e22
          shipMass = 1.0e6
          planetRadius = 6_371_000
          orbitRadius = 384_402_000
          moonVelocity :: Velocity
          moonVelocity = (0, sqrt $ gravitationalConstant * planetMass / orbitRadius)
          shipVelocityMagnitude = (0.8 *) . sqrt $ gravitationalConstant * planetMass / planetRadius
          velocityDir = Vec.unitVectorAtAngle angle
          shipVelocity = shipVelocityMagnitude Vec.* velocityDir
          positionDir = Vec.unitVectorAtAngle $ -0.2
          shipInitialPosition = (orbitRadius * 0.7) Vec.* positionDir
    -- where planetMass = 7.348e22
    --       moonMass = 7.348e2
    --       shipMass = 1.0e6
    --       planetRadius = 6_371_000
    --       orbitRadius = 384_402_000
    --       moonVelocity :: Velocity
    --       moonVelocity = (0, sqrt $ gravitationalConstant * planetMass / orbitRadius)
    --       shipVelocityMagnitude = (0.8 *) . sqrt $ gravitationalConstant * planetMass / planetRadius
    --       velocityDir = Vec.unitVectorAtAngle $ 0.77 + pi - 0.055
    --       shipVelocity = shipVelocityMagnitude Vec.* velocityDir
    --       positionDir = Vec.unitVectorAtAngle $ 0.77
    --       shipInitialPosition = (orbitRadius * 0.25) Vec.* positionDir

formatFloat :: Int -> Float -> String
formatFloat n x = showEFloat (Just n) x ""

render :: SimulationState -> Picture
render state = pictures
    [ renderObject yellow $ planet state
    , renderObject white  $ moon state
    , tr
    , renderObject blue   $ ship state
    , translate (-fromIntegral width / 2) (-fromIntegral height / 2) $ scale 0.2 0.2 $ color white $ text $ 
        -- show $ ((join (***) $ \x -> showEFloat (Just 4) x "") $ velocity $ planet state)
        (formatFloat 4 $ Vec.magV $ velocity $ ship state) ++ " m/s"
        -- show $ lastTraceUpdate state
    ]
    where
        scaleDistance :: Position -> Position
        scaleDistance = (7.0e-7 Vec.*)
        tr = color red $ line $ map scaleDistance $ trace state
        renderObject :: Color -> Object -> Picture
        renderObject c Object { mass, position } =
            uncurry translate (scaleDistance position) $ color c $ circleSolid $ max 3 (1.0e-7 * mass ** (1/3)) -- (2 * log mass / log 20)


type Force = Vec.Vector

force :: Object -> Object -> Force
force Object { mass = mass1, position = pos1 } Object { mass = mass2, position = pos2 } =
    if distance < 1e-3 then (0.0, 0.0)
    else gravitationalConstant * mass1 * mass2 / (distance * distance) Vec.* u
    where delta :: Vec.Vector
          delta = pos2 Vec.- pos1
          distance = Vec.magV delta
          u = Vec.normalizeV delta

{--
distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) = let dx = x2 - x1
                                 dy = y2 - y1
                             in sqrt $ dx * dx + dy * dy
--}

type Acceleration = Vec.Vector

acceleration :: Mass -> Force -> Acceleration
acceleration mass f = recip mass Vec.* f

type Time = Float
timeScale :: Time
timeScale = 30

accelerate :: Acceleration -> Time -> Object -> Object
accelerate a t obj@Object { velocity } = obj { velocity = velocity Vec.+ (t Vec.* a) }

move :: Time -> Object -> Object
move time obj@Object { position, velocity } = obj { position = position Vec.+ time Vec.* velocity }

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ = id
applyNTimes n f = f . applyNTimes (n - 1) f

update :: ViewPort -> Time -> SimulationState -> SimulationState
update _ time = applyNTimes (floor (1e2 :: Float)) $ update' $ timeScale * time

update' :: Time -> SimulationState -> SimulationState
update' seconds sim@Simulation { objects, trace } = sim''
    where accs :: Object -> [Object] -> Acceleration
          accs obj = foldr1 (Vec.+) . map f
              where f :: Object -> Acceleration
                    f = acceleration (mass obj) . force obj
          time = seconds
          objs' = map (\o -> accelerate (accs o objects) time o) objects
          objs'' = map (move time) objs'
          sim' = sim { objects = objs'', lastTraceUpdate = lastTraceUpdate sim + seconds / timeScale }
          sim'' = if lastTraceUpdate sim' > 5
                  then sim' { trace = position (ship sim') : take 2000 trace, lastTraceUpdate = 0 }
                  else sim'

fps :: Int
fps = 60

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show args
    let angle = if "d" `elem` args then deceleratingAngle else acceleratingAngle
    putStrLn $ show angle
    simulate window background fps (initialState angle) render update

