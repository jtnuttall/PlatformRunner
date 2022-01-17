module PlatformRunner.Level.Internal.STM
  ( LevelProducer
  , InitialLevelParameters(..)
  , PullUpdate
  , ItemDescriptorResult(..)
  , toLevelProducer
  , withLevel
  ) where

import           Common.Import
import           Conduit
import           Control.Concurrent.STM.TBMQueue
                                                ( TBMQueue
                                                , newTBMQueue
                                                , tryReadTBMQueue
                                                , writeTBMQueue
                                                )
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.RWS
import           Control.Monad.Zip
import           GHC.Generics                   ( Generic1 )
import           Linear
import           PlatformRunner.Level.Internal.Types

inQueueSize :: Int
inQueueSize = 50

data InitialLevelParameters m = InitialLevelParameters
  { initialRequest :: Double
  , metadata       :: LevelMetadata
  , producer       :: LevelProducer m
  }

data ItemDescriptorResult
  = Closed
  | Waiting
  | Result !(Vector RelativeItemDescriptor)
  deriving Show

data LevelMail = LevelMail
  { levelDataQueue   :: !LevelDataQueue
  , levelDataRequest :: !LevelDataRequest
  }

newtype LevelProducer m = LevelProducer
  { unLevelProducer :: SealedConduitT () RelativeItemDescriptor m ()}

newtype LevelDataRequest = LevelDataRequest
  { unLevelDataRequest :: TMVar Double }

newtype LevelDataQueue = LevelDataQueue
  { unLevelDataQueue :: TBMQueue (Vector RelativeItemDescriptor) }

newLevelDataQueue :: STM LevelDataQueue
newLevelDataQueue = LevelDataQueue <$> newTBMQueue inQueueSize

pullLevelData
  :: LevelDataRequest -> LevelDataQueue -> Double -> STM ItemDescriptorResult
pullLevelData req (LevelDataQueue queue) requestSize = do
  res <- tryReadTBMQueue queue

  case res of
    Nothing               -> return Closed
    Just Nothing          -> requestLevelData req requestSize >> return Waiting
    Just (Just levelData) -> return (Result levelData)

pushLevelData :: LevelDataQueue -> Vector RelativeItemDescriptor -> STM ()
pushLevelData (LevelDataQueue queue) = writeTBMQueue queue

newLevelDataRequest :: Double -> STM LevelDataRequest
newLevelDataRequest initialPullDistance =
  LevelDataRequest <$> newTMVar initialPullDistance

requestLevelData :: LevelDataRequest -> Double -> STM Bool
requestLevelData (LevelDataRequest ref) = tryPutTMVar ref

waitForLevelDataRequest :: LevelDataRequest -> STM Double
waitForLevelDataRequest (LevelDataRequest ref) = takeTMVar ref

toLevelProducer :: ConduitT () RelativeItemDescriptor m () -> LevelProducer m
toLevelProducer = LevelProducer . sealConduitT

produce :: (MonadIO m, PrimMonad m) => LevelProducer m -> LevelMail -> m ()
produce (LevelProducer initialProducer) LevelMail {..} =
  let loop producer = do
        newRequest <- atomically $ waitForLevelDataRequest levelDataRequest
        let checkRange desc newDist | newDist > newRequest = Left newDist
                                    | otherwise = Right (newDist, desc)

            accumUntilNewEnd desc dist = case desc of
              PlatformDescriptor { offset } ->
                checkRange desc (dist + (offset ^. _x))

            sink = void (mapAccumWhileC accumUntilNewEnd 0) .| sinkVector

        (producer', newItems) <- producer $$++ sink

        atomically $ pushLevelData levelDataQueue newItems

        loop producer'
  in  loop initialProducer

type PullUpdate m = Double -> m ItemDescriptorResult

withLevel
  :: (MonadUnliftIO m, PrimMonad m)
  => InitialLevelParameters m
  -> (LevelMetadata -> PullUpdate m -> m ())
  -> m ()
withLevel (InitialLevelParameters initialRequest levelMetadata levelProducer) subscribe
  = do
    levelDataRequest <- atomically $ newLevelDataRequest initialRequest
    levelDataQueue   <- atomically newLevelDataQueue

    let pullUpdate d =
          atomically $ pullLevelData levelDataRequest levelDataQueue d
        levelMail = LevelMail { levelDataQueue, levelDataRequest }

    race_ (produce levelProducer levelMail) (subscribe levelMetadata pullUpdate)
