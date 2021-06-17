module Main where

import Prelude

import Data.Ball (Ball)
import Data.Ball as Ball
import Data.Maybe (Maybe(..))
import Data.Vec as Vec
import Effect (Effect)
import Effect.Console (log)
import Effect.Timer as Timer
import Graphics.Canvas (CanvasElement, Context2D, Dimensions)
import Graphics.Canvas as Canvas
import Graphics.Canvas.Extra as Canvas


canvasDimen :: Dimensions
canvasDimen =
  { width: 600.0 , height: 600.0
  }


initCanvas :: CanvasElement -> Effect Unit
initCanvas canvas =
  Canvas.setCanvasDimensions canvas canvasDimen


type Model = Ball
  
init :: Model
init =
  Ball.new
    (Vec.new
        (canvasDimen.width / 2.0)
        (canvasDimen.height / 2.0))
    (Vec.new 10.0 2.0)
    10.0
    10.0


update :: Model -> Model
update ball =
  Ball.update canvasDimen ball


view :: Context2D -> Model -> Effect Unit
view ctx ball = do
  Canvas.setFillStyle ctx "salmon"
  Canvas.fillPath ctx $
    Canvas.circle ctx $ Ball.toCircle ball
    


mainLoop :: Context2D -> Model -> Effect Unit
mainLoop ctx model = do
  Canvas.clearCanvas ctx canvasDimen
  view ctx model
  void $ Timer.setTimeout 10 $ mainLoop ctx (update model)


main :: Effect Unit
main = 
  Canvas.getCanvasElementById "canvas" >>=
    case _ of
      Just canvas -> do
         initCanvas canvas
         ctx <- Canvas.getContext2D canvas
         mainLoop ctx init

      Nothing ->
        log "Error on getCanvasElementById."
