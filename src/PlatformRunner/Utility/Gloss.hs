module PlatformRunner.Utility.Gloss where
-- More generic versions of gloss functions from Apecs

import           Apecs
import           Apecs.Gloss                    ( Display
                                                , Picture
                                                , cameraTransform
                                                , playIO
                                                )
import           Apecs.Physics.Gloss            ( Camera
                                                , Color
                                                , Event
                                                )
import           RIO                     hiding ( Display )

-- | Renders a picture given a component rendering function.
foldDraw
  :: (Get w m c, Members w m c)
  => (c -> Picture) -- ^ Component render function.
  -> SystemT w m Picture
foldDraw fn = cfold (\pic -> mappend pic . fn) mempty

-- | Monadically renders a picture given a component rendering function.
foldDrawM
  :: (MonadIO m, Get w m c, Members w m c)
  => (c -> SystemT w m Picture) -- ^ Component render function.
  -> SystemT w m Picture
foldDrawM fn = cfoldM (\pic -> fmap (mappend pic) . fn) mempty

play
  :: (MonadUnliftIO m, Has w m Camera)
  => Display                   -- ^ Display mode
  -> Color                     -- ^ Background color
  -> Int                       -- ^ Desired FPS
  -> SystemT w m Picture       -- ^ Drawing function
  -> (Event -> SystemT w m ()) -- ^ Event handling function
  -> (Float -> SystemT w m ()) -- ^ Stepping function, with a time delta argument.
  -> SystemT w m ()
play disp col fps draw handleEvent step = do
  world <- ask

  let handleEvent' event w' = runWith w' $ handleEvent event >> ask
      step' dT w' = runWith w' $ step dT >> ask
      draw' w' = runWith w' $ do
        cam <- get global
        cameraTransform cam <$> draw

  lift $ withRunInIO $ \unlift -> playIO
    disp
    col
    fps
    world
    (unlift . draw')
    (\event' w' -> unlift (handleEvent' event' w'))
    (\dT world' -> unlift (step' dT world'))
