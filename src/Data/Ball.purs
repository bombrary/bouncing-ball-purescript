module Data.Ball where

import Prelude

import Control.Monad.State (State, execState, gets, modify_)
import Data.Tuple (Tuple(..))
import Data.Vec (Vec)
import Data.Vec as Vec
import Graphics.Canvas (Dimensions)
import Graphics.Canvas.Extra (Circle)


newtype Ball = Ball
  { r :: Vec
  , v :: Vec
  , radius :: Number
  , force :: Vec
  , mass :: Number
  }


getForce :: Ball -> Vec
getForce (Ball { force }) = force


getMass :: Ball -> Number
getMass (Ball { mass }) = mass


addForce :: Vec -> Ball -> Ball
addForce newForce (Ball b@{ force }) =
  Ball b { force = Vec.add force newForce }


initForce :: Ball -> Ball
initForce (Ball b) =
  Ball b { force = Vec.zero }


applyForce :: Ball -> Ball
applyForce ball =
  let a = Vec.div (getForce ball) (getMass ball)
      newV = Vec.add (getV ball) a
  in
    initForce $ setV newV ball


new :: Vec -> Vec -> Number -> Number -> Ball
new r v radius mass =
  Ball
    { r
    , v
    , radius
    , force: Vec.zero
    , mass
    }


instance Show Ball where
  show (Ball { r, v, radius }) =
    "Ball (" <> show r <> ")(" <> show v <> ")(" <> show radius <>")"


getR :: Ball -> Vec
getR (Ball {r}) = r


getV :: Ball -> Vec
getV (Ball {v}) = v


setR :: Vec -> Ball -> Ball
setR r (Ball b) = Ball b { r = r }


setV :: Vec -> Ball -> Ball
setV v (Ball b) = Ball b { v = v }


getRadius :: Ball -> Number
getRadius (Ball {radius}) = radius


toCircle :: Ball -> Circle
toCircle (Ball {r, radius}) =
  { x: Vec.getX r
  , y: Vec.getY r
  , radius
  }


move :: Ball -> Ball
move ball =
  setR
    (Vec.add (getR ball) (getV ball))
    ball

{-
 update :: Dimensions -> Ball -> Ball
 update dimen ball =
   applyForce $
     move $
       collide 0.9 dimen $
         addForce (gravity (getMass ball)) ball
-}

update :: Dimensions -> Ball -> Ball
update dimen ball = execState state ball
  where
    state :: State Ball Unit
    state = do
      modify_ $ addForce (gravity (getMass ball))
      modify_ $ collide 0.9 dimen
      modify_ $ move
      modify_ $ applyForce


gravity :: Number -> Vec
gravity mass =
  Vec.mult mass $ Vec.new 0.0 0.25


collide :: Number -> Dimensions -> Ball -> Ball
collide e dimen ball = execState state ball
  where
    state :: State Ball Unit
    state = do
       r <- gets getR
       v <- gets getV
       radius <- gets getRadius 
       let x = Vec.getX r
           y = Vec.getY r
           vx = Vec.getX v
           vy = Vec.getY v
           x0 = radius
           x1 = dimen.width - radius
           y0 = radius
           y1 = dimen.height - radius
       when (x < x0) do
         modify_ $ setR (Vec.setX x0 r)
         modify_ $ setV (Vec.setX (-e * vx) v)
       when (x > x1) do
         modify_ $ setR (Vec.setX x1 r)
         modify_ $ setV (Vec.setX (-e * vx) v)
       when (y < y0) do
         modify_ $ setR (Vec.setY y0 r)
         modify_ $ setV (Vec.setY (-e * vy) v)
       when (y > y1) do
         modify_ $ setR (Vec.setY y1 r)
         modify_ $ setV (Vec.setY (-e * vy) v)
