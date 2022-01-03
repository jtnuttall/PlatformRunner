module PlatformRunner.Game.Input where

import           Apecs
import           Apecs.Gloss
import           PlatformRunner.Env             ( HasGameConstantsRef
                                                  ( gameConstantsRefL
                                                  )
                                                )
import           PlatformRunner.Game.Constant
import           PlatformRunner.Import   hiding ( Down )

handleEvent
  :: (HasGameConstantsRef env) => Event -> PlatformRunnerSystem env ()
handleEvent event = do
  constants <- lift $ readSomeRef =<< view gameConstantsRefL

  case event of
    EventKey (Char 'w') Down _ _ -> cmap
      $ \(Player, Velocity v0) -> Velocity (v0 + playerJumpVelocity constants)

    _ -> return ()
