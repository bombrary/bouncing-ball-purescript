module Data.Vec where

import Prelude
import Math as Math


newtype Vec = Vec
  { x :: Number
  , y :: Number
  }


instance Show Vec where
  show (Vec {x, y}) = "Vec " <> show x <> ", " <> show y


getX :: Vec -> Number
getX (Vec {x}) = x


getY :: Vec -> Number
getY (Vec {y}) = y


setX :: Number -> Vec -> Vec
setX xnew (Vec v) =
  Vec v { x = xnew }


setY :: Number -> Vec -> Vec
setY ynew (Vec v) =
  Vec v { y = ynew }


new :: Number -> Number -> Vec
new x y = Vec { x, y }


zero :: Vec
zero = Vec { x: 0.0, y: 0.0 }


add :: Vec -> Vec -> Vec
add (Vec u) (Vec v) =
  new (u.x + v.x) (u.y + v.y)


sub :: Vec -> Vec -> Vec
sub (Vec u) (Vec v) =
  new (u.x - v.x) (u.y - v.y)


mult :: Number -> Vec -> Vec
mult c (Vec v) =
  new (c * v.x) (c * v.y)


div :: Vec -> Number -> Vec
div (Vec v) c =
  new (v.x / c) (v.y / c)


magSq :: Vec -> Number
magSq (Vec v) =
  v.x * v.x + v.y * v.y


mag :: Vec -> Number
mag v = Math.sqrt $ magSq v
