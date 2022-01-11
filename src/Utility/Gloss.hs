module Utility.Gloss where

import           Apecs
import           Apecs.Gloss                    ( Display
                                                , Event(..)
                                                , Picture
                                                , blue
                                                , cameraTransform
                                                , color
                                                , green
                                                , playIO
                                                , red
                                                )
import           Apecs.Physics
import           Apecs.Physics.Gloss            ( Camera
                                                , Color
                                                , Transform
                                                , convexToPicture
                                                , worldTransform
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

drawBody
  :: (MonadIO m, Has w m Physics)
  => (Body, Transform, ShapeList)
  -> SystemT w m Picture
drawBody (btype, transform, ShapeList shapes) =
  color shColor . worldTransform transform <$> foldM foldfn mempty shapes
 where
  foldfn pic shapeEty = do
    Shape _ convex <- get shapeEty
    return . mappend pic $ convexToPicture convex
  shColor = case btype of
    DynamicBody   -> red
    KinematicBody -> green
    StaticBody    -> blue

-- | Unlifted version of playIO
play
  :: (MonadUnliftIO m, Has w m Camera)
  => Display                   -- ^ Display mode
  -> Color                     -- ^ Background color
  -> Int                       -- ^ Desired FPS
  -> SystemT w m Picture       -- ^ Drawing function
  -> (Event -> SystemT w m ()) -- ^ Event handling function
  -> (Double -> SystemT w m ()) -- ^ Stepping function, with a time delta argument.
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
    (\dT world' -> unlift (step' (realToFrac dT) world'))
