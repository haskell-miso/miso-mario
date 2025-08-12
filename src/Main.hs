-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
data Action
  = GetArrows !Arrows
  | Time !Double
  | Start
-----------------------------------------------------------------------------
spriteFrames :: [MisoString]
spriteFrames =
  [ "0 0"
  , "-74px 0"
  , "-111px 0"
  , "-148px 0"
  , "-185px 0"
  , "-222px 0"
  , "-259px 0"
  , "-296px 0"
  ]
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ do
  time <- now
  let m = defaultMario { time = time }
  startApp (component m updateMario viewMario)
    { subs =
        [ arrowsSub GetArrows
        ]
    , initialAction = Just Start
    }
-----------------------------------------------------------------------------
data Mario = Mario
  { x,y :: !Double
  , vx,vy :: !Double
  , time, delta :: !Double
  , dir :: !Direction
  , arrows :: !Arrows
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Direction
  = L
  | R
  deriving (Show, Eq)
-----------------------------------------------------------------------------
defaultMario :: Mario
defaultMario = Mario
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = R
  , time = 0
  , delta = 0
  , arrows = Arrows 0 0
  }
-----------------------------------------------------------------------------
updateMario :: Action -> Transition Mario Action
updateMario Start = get >>= step
updateMario (GetArrows arrs) = do
  modify newMario
  step =<< get
    where
      newMario m = m { arrows = arrs }
updateMario (Time newTime) = do
  modify newMario
  step =<< get
    where
      newMario m = m
        { delta = (newTime - time m) / 20
        , time = newTime
        }
-----------------------------------------------------------------------------
-- | Helper lens for 'Mario'
mario :: Lens Mario Mario
mario = this
-----------------------------------------------------------------------------
step :: Mario -> Transition Mario Action
step Mario{..} = do
  mario %= gravity delta
  mario %= jump arrows
  mario %= walk arrows
  mario %= physics delta
  io (Time <$> now)
-----------------------------------------------------------------------------
jump :: Arrows -> Mario -> Mario
jump Arrows{..} m@Mario{..}
  | arrowY > 0 && vy == 0 = m { vy = 6 }
  | otherwise = m
-----------------------------------------------------------------------------
gravity :: Double -> Mario -> Mario
gravity dt m@Mario{..} = m { vy = if y > 0 then vy - (dt / 4) else 0 }
-----------------------------------------------------------------------------
physics :: Double -> Mario -> Mario
physics dt m@Mario{..}
  = m
  { x = x + dt * vx
  , y = max 0 (y + dt * vy)
  }
-----------------------------------------------------------------------------
walk :: Arrows -> Mario -> Mario
walk Arrows{..} m@Mario{..}
  = m
  { vx = fromIntegral arrowX
  , dir = if | arrowX < 0 -> L
             | arrowX > 0 -> R
             | otherwise -> dir
  }
-----------------------------------------------------------------------------
viewMario :: Mario -> View model Action
viewMario m = marioImage
  where
    groundY = -400
    marioImage =
        div_
        [ height_ "400"
        , width_ "400"
        ]
        [ nodeHtml "style" []
          [ "@keyframes play { 100% { background-position: -296px; } }"
            -- dmj: use keyframes DSL for this
          ]
        , div_
          [ CSS.style_ (marioStyle m groundY)
          ]
          []
        ]
-----------------------------------------------------------------------------
marioStyle :: Mario -> Double -> [CSS.Style]
marioStyle Mario{..} gy =
  [ CSS.transform $ matrix dir x $ abs (y + gy)
  , CSS.display "block"
  , CSS.width (CSS.px 37)
  , CSS.height (CSS.px 37)
  , CSS.backgroundColor CSS.transparent
  , CSS.backgroundImage (CSS.url "assets/mario.png")
  , CSS.backgroundRepeat "no-repeat"
  , CSS.backgroundPosition (spriteFrames !! frame)
  ] ++
  [ CSS.animation "play 0.8s steps(8) infinite"
  | y == 0 && vx /= 0
  ] where
      frame
        | y > 0 = 1
        | otherwise = 0
-----------------------------------------------------------------------------
matrix :: Direction -> Double -> Double -> MisoString
matrix dir x y = CSS.matrix (if dir == L then -1 else 1) 0 0 1 x y
-----------------------------------------------------------------------------
