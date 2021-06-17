module Graphics.Canvas.Extra where

import Prelude
import Effect (Effect)
import Graphics.Canvas (Context2D, Dimensions)
import Graphics.Canvas as Canvas
import Math as Math


clearCanvas :: Context2D -> Dimensions -> Effect Unit
clearCanvas ctx dimen =
  Canvas.clearRect ctx
    { x: 0.0
    , y: 0.0
    , width: dimen.width
    , height: dimen.height
    }


type Circle =
  { x :: Number
  , y :: Number
  , radius :: Number
  }


circle :: Context2D -> Circle -> Effect Unit
circle ctx {x, y, radius} =
  Canvas.arc ctx
    { x
    , y
    , radius
    , start: 0.0
    , end: Math.tau
    }
