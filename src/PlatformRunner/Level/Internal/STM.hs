module PlatformRunner.Level.Internal.STM where

import           Conduit
import           Control.Concurrent.STM.TBMQueue
                                                ( TBMQueue
                                                , newTBMQueue
                                                , tryReadTBMQueue
                                                , writeTBMQueue
                                                )
import           Linear
import           PlatformRunner.Types
import           RIO

inQueueSize = 50

data RelativeItemDescriptor = PlatformDescriptor
  { offset  :: !(V2 Double)
  , extents :: !Dimensions
  }
  deriving Show

data ProceduralData = ProceduralData
  { proceduralSeed :: Vector Word8
  , proceduralAlgo :: Maybe Text
  }
  deriving Show

data WinCondition
  = Distance Double
  | DistanceInTime Double Double
  | Platform Int
  | None
  deriving Show

data LevelMetadata = LevelMetadata
  { levelDataPath       :: !(Maybe FilePath)
  , levelName           :: !Text
  , levelDescription    :: Maybe Text
  , levelProcedural     :: !(Maybe ProceduralData)
  , levelPlayerStartPos :: !(V2 Double)
  , winCondition        :: !WinCondition
  }
  deriving Show

newtype LevelDataQueue = LevelDataQueue
  { unLevelDataQueue :: TBMQueue (Vector RelativeItemDescriptor) }

newLevelDataQueue :: STM LevelDataQueue
newLevelDataQueue = LevelDataQueue <$> newTBMQueue inQueueSize

pullLevelData
  :: LevelDataQueue -> STM (Maybe (Maybe (Vector RelativeItemDescriptor)))
pullLevelData (LevelDataQueue queue) = tryReadTBMQueue queue

pushLevelData :: LevelDataQueue -> Vector RelativeItemDescriptor -> STM ()
pushLevelData (LevelDataQueue queue) = writeTBMQueue queue

newtype LevelDataRequest = LevelDataRequest
  { unLevelDataRequest :: TMVar Double }

newLevelDataRequest :: Double -> STM LevelDataRequest
newLevelDataRequest initialPullDistance =
  LevelDataRequest <$> newTMVar initialPullDistance

requestLevelData :: LevelDataRequest -> Double -> STM Bool
requestLevelData (LevelDataRequest ref) = tryPutTMVar ref

waitForLevelDataRequest :: LevelDataRequest -> STM Double
waitForLevelDataRequest (LevelDataRequest ref) = takeTMVar ref

-- waitForRequest :: 

data LevelMail = LevelMail
  { levelDataQueue   :: !LevelDataQueue
  , levelDataRequest :: !LevelDataRequest
  }

newtype LevelProducer m = LevelProducer
  { unLevelProducer :: SealedConduitT () RelativeItemDescriptor m ()}

-- createLevelSTM :: MonadIO m => LevelMetadata -> LevelProducer m r -> STM Level
-- createLevelSTM metadata producer = undefined

data InitialLevelParameters = InitialLevelParameters
  { initialRequest :: Double
  , metadata       :: LevelMetadata
  , producer       :: forall m . Monad m => LevelProducer m
  }

sinkItemChunks
  :: (PrimMonad m)
  => ConduitT RelativeItemDescriptor Void m (Vector RelativeItemDescriptor)
sinkItemChunks =
  let accumUntilTerminus = undefined
      lastPlatformPos    = V2 0 0
  in  void (mapAccumWhileC accumUntilTerminus lastPlatformPos) .| sinkVector

subscribe :: (MonadIO m, PrimMonad m) => LevelMail -> LevelProducer m -> m ()
subscribe LevelMail {..} (LevelProducer initialProducer) =
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

type RequestMore m = MonadIO m => Double -> m Bool

type PullUpdate m
  = MonadIO m => m (Maybe (Maybe (Vector RelativeItemDescriptor)))

withLevel
  :: (MonadUnliftIO m, PrimMonad m)
  => InitialLevelParameters
  -> (LevelMetadata -> RequestMore m -> PullUpdate m -> m ())
  -> m ()
withLevel (InitialLevelParameters initialRequest levelMetadata levelProducer) go
  = do
    levelDataRequest <- atomically $ newLevelDataRequest initialRequest
    levelDataQueue   <- atomically newLevelDataQueue

    let requestMore = atomically . requestLevelData levelDataRequest
        pullUpdate  = atomically $ pullLevelData levelDataQueue

    concurrently_
      (subscribe LevelMail { levelDataQueue, levelDataRequest } levelProducer)
      (go levelMetadata requestMore pullUpdate)
