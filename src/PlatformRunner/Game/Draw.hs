module PlatformRunner.Game.Draw where

import           Apecs
import           Apecs.Gloss             hiding ( foldDraw
                                                , foldDrawM
                                                )
import           Apecs.Physics           hiding ( Position
                                                , Velocity
                                                )
import           PlatformRunner.Game.Constant   ( PlatformRunnerSystem )
import           PlatformRunner.Import
import           PlatformRunner.Utility.Gloss

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

triangle, diamond :: Picture
triangle = Line [(0, 0), (-0.5, -1), (0.5, -1), (0, 0)]
diamond = Line [(-1, 0), (0, -1), (1, 0), (0, 1), (-1, 0)]


draw :: PlatformRunnerSystem env Picture
draw = do
  player <- foldDraw
    $ \(Player, pos) -> translate' pos . color white . scale 10 20 $ triangle

  particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
    translate' pos . color orange $ Line [(0, 0), (vx / 10, vy / 10)]

  Score s <- get global
  let score =
        color white
          .  translate' (Position $ V2 10 10)
          .  scale 0.1 0.1
          .  Text
          $  "Score: "
          ++ show s

  return $ player <> score <> particles
