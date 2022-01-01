module PlatformRunner.Game.Input where

import           Apecs
import           Apecs.Gloss
import           PlatformRunner.Game.Constant
import           PlatformRunner.Import   hiding ( Down )

handleEvent :: PlatformRunnerConstants -> Event -> PlatformRunnerSystem env ()
handleEvent constants = \case
  EventKey (Char 'w') Down _ _ -> cmap
    $ \(Player, Velocity v0) -> Velocity (v0 + playerJumpVelocity constants)
  _ -> error "unimpl"
