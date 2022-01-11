module PlatformRunner.Env where

import           Control.Lens            hiding ( view )
import           PlatformRunner.Game.Constant   ( PlatformRunnerConstants )
import           PlatformRunner.Prelude
import           PlatformRunner.Settings.Internal
                                                ( Settings(..) )
import           PlatformRunner.Types           ( Dimensions )
import           RIO.Process                    ( HasProcessContext(..)
                                                , ProcessContext
                                                )


--------------------------------------------------------------------------------
class HasConfigElem config prop where
  configElemL :: Lens' config prop


viewConfig
  :: forall prop config m
   . HasConfigElem config prop
  => MonadReader config m => m prop
viewConfig = view $ configElemL @config

instance (HasLogFunc env) => HasConfigElem env LogFunc where
  configElemL = logFuncL

instance (HasProcessContext env) => HasConfigElem env ProcessContext where
  configElemL = processContextL

--------------------------------------------------------------------------------
newtype OptionVerboseFlag = OptionVerboseFlag
  { unOptionVerboseFlag :: Maybe Bool }
  deriving Show

instance HasConfigElem OptionVerboseFlag OptionVerboseFlag where
  configElemL = id

newtype OptionOverrideSettingsPath = OptionOverrideSettingsPath
  { unOverrideSettingsPath :: Maybe FilePath }
  deriving Show

instance HasConfigElem OptionOverrideSettingsPath OptionOverrideSettingsPath where
  configElemL = id

-- | Represents the app's possible CLI Options. Used by Optparse Simple
data CliOptions = CliOptions
  { optionVerboseFlag      :: !OptionVerboseFlag
  , optionOverrideSettings :: !OptionOverrideSettingsPath
  }
  deriving Show

instance HasConfigElem CliOptions CliOptions where
  configElemL = id

instance HasConfigElem CliOptions OptionVerboseFlag where
  configElemL = lens optionVerboseFlag (\x y -> x { optionVerboseFlag = y })

instance HasConfigElem CliOptions OptionOverrideSettingsPath where
  configElemL =
    lens optionOverrideSettings (\x y -> x { optionOverrideSettings = y })

--------------------------------------------------------------------------------
newtype ScreenSize = ScreenSize { unScreenSize :: Dimensions } deriving Show

instance HasConfigElem ScreenSize ScreenSize where
  configElemL = id

newtype ConfigDir = ConfigDir { unConfigDir :: FilePath } deriving Show

instance HasConfigElem ConfigDir ConfigDir where
  configElemL = id

-- | Represents the basic environment of the app, available immediately
-- on entry into main
data AppEnv = AppEnv
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appCliOptions     :: !CliOptions
  , appScreenSize     :: !ScreenSize
  , appConfigDir      :: !ConfigDir
  }

instance HasConfigElem AppEnv AppEnv where
  configElemL = id

instance HasLogFunc AppEnv where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext AppEnv where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

instance HasConfigElem AppEnv CliOptions where
  configElemL = lens appCliOptions (\x y -> x { appCliOptions = y })

instance HasConfigElem AppEnv OptionVerboseFlag where
  configElemL = configElemL . configElemL @CliOptions @OptionVerboseFlag

instance HasConfigElem AppEnv OptionOverrideSettingsPath where
  configElemL =
    configElemL . configElemL @CliOptions @OptionOverrideSettingsPath

instance HasConfigElem AppEnv ScreenSize where
  configElemL = lens appScreenSize (\x y -> x { appScreenSize = y })

instance HasConfigElem AppEnv ConfigDir where
  configElemL = lens appConfigDir (\x y -> x { appConfigDir = y })


--------------------------------------------------------------------------------
type SettingsRef = SomeRef Settings

instance HasConfigElem SettingsRef SettingsRef where
  configElemL = id

-- | Represents the basic configuration available to the app without reading
-- any particular files
data AppBaseSettingsEnv = AppBaseSettingsEnv
  { appEnv         :: !AppEnv
  , appSettingsRef :: !(SomeRef Settings)
  }

instance HasConfigElem AppBaseSettingsEnv AppBaseSettingsEnv where
  configElemL = id

instance HasConfigElem AppBaseSettingsEnv AppEnv where
  configElemL = lens appEnv (\x y -> x { appEnv = y })

instance HasLogFunc AppBaseSettingsEnv where
  logFuncL = configElemL . configElemL @AppEnv @LogFunc

instance HasProcessContext AppBaseSettingsEnv where
  processContextL = configElemL . configElemL @AppEnv @ProcessContext

instance HasConfigElem AppBaseSettingsEnv CliOptions where
  configElemL = configElemL . configElemL @AppEnv @CliOptions

instance HasConfigElem AppBaseSettingsEnv OptionVerboseFlag where
  configElemL = configElemL . configElemL @AppEnv @OptionVerboseFlag

instance HasConfigElem AppBaseSettingsEnv OptionOverrideSettingsPath where
  configElemL = configElemL . configElemL @AppEnv @OptionOverrideSettingsPath

instance HasConfigElem AppBaseSettingsEnv ScreenSize where
  configElemL = configElemL . configElemL @AppEnv @ScreenSize

instance HasConfigElem AppBaseSettingsEnv ConfigDir where
  configElemL = configElemL . configElemL @AppEnv @ConfigDir

instance HasConfigElem AppBaseSettingsEnv SettingsRef where
  configElemL = lens appSettingsRef (\x y -> x { appSettingsRef = y })

--------------------------------------------------------------------------------
-- | Represents the complete environment required to run the app
data PlatformRunnerEnv = PlatformRunnerEnv
  { appBaseSettingsEnv :: !AppBaseSettingsEnv
  , gameConstants      :: !PlatformRunnerConstants
  }

instance HasConfigElem PlatformRunnerEnv PlatformRunnerEnv where
  configElemL = id

instance HasConfigElem PlatformRunnerEnv AppBaseSettingsEnv where
  configElemL = lens appBaseSettingsEnv (\x y -> x { appBaseSettingsEnv = y })

instance HasConfigElem PlatformRunnerEnv AppEnv where
  configElemL = configElemL . configElemL @AppBaseSettingsEnv @AppEnv

instance HasLogFunc PlatformRunnerEnv where
  logFuncL = configElemL . configElemL @AppEnv @LogFunc

instance HasProcessContext PlatformRunnerEnv where
  processContextL = configElemL . configElemL @AppEnv @ProcessContext

instance HasConfigElem PlatformRunnerEnv CliOptions where
  configElemL = configElemL . configElemL @AppEnv @CliOptions

instance HasConfigElem PlatformRunnerEnv OptionVerboseFlag where
  configElemL = configElemL . configElemL @CliOptions @OptionVerboseFlag

instance HasConfigElem PlatformRunnerEnv OptionOverrideSettingsPath where
  configElemL =
    configElemL . configElemL @CliOptions @OptionOverrideSettingsPath

instance HasConfigElem PlatformRunnerEnv ScreenSize where
  configElemL = configElemL . configElemL @AppEnv @ScreenSize

instance HasConfigElem PlatformRunnerEnv ConfigDir where
  configElemL = configElemL . configElemL @AppEnv @ConfigDir

instance HasConfigElem PlatformRunnerEnv SettingsRef where
  configElemL = configElemL . configElemL @AppBaseSettingsEnv @SettingsRef

instance HasConfigElem PlatformRunnerEnv PlatformRunnerConstants where
  configElemL = lens gameConstants (\x y -> x { gameConstants = y })

--------------------------------------------------------------------------------
instance HasStateRef Settings AppBaseSettingsEnv where
  stateRefL = lens appSettingsRef (\x y -> x { appSettingsRef = y })

instance HasStateRef Settings PlatformRunnerEnv where
  stateRefL = configElemL @PlatformRunnerEnv @SettingsRef . stateRefL
