module PlatformRunner.Game.Draw where

import           Apecs
import           Apecs.Gloss             hiding ( foldDraw
                                                , foldDrawM
                                                )
import           Apecs.Physics
import           Common.Import
import           PlatformRunner.Components.Entity
import           PlatformRunner.Components.Global
import           PlatformRunner.Game.Constant   ( PlatformRunnerSystem )
import           Utility.Gloss

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate (realToFrac x) (realToFrac y)

triangle :: Picture
triangle = Line [(0, 0), (-0.5, -1), (0.5, -1), (0, 0)]

diamond :: Picture
diamond = Line [(-1, 0), (0, -1), (1, 0), (0, 1), (-1, 0)]

draw :: PlatformRunnerSystem env Picture
draw = do
  player <- foldDrawM $ \(Player, body, transform, shapeList) ->
    drawBody (body, transform, shapeList)

  platforms <- foldDrawM $ \(Platform, body, transform, shapeList) ->
    drawBody (body, transform, shapeList)

  particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
    translate' pos . color orange $ Line
      [(0, 0), (realToFrac vx / 10, realToFrac vy / 10)]

  Score s <- get global
  let score =
        color white
          .  translate' (Position $ V2 10 10)
          .  scale 0.1 0.1
          .  Text
          $  "Score: "
          ++ show s

  return $ player <> platforms <> score <> particles
