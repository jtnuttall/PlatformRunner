module PlatformRunner.AppEnv where

import           PlatformRunner.Settings.Types  ( Settings )
import           RIO
import           RIO.Process

-- TODO use TemplateHaskell for Display instances

-- | Represents the app's possible CLI Options. Used by Optparse Simple
data CliOptions = CliOptions
  { optionsVerbose          :: !Bool
  , optionsOverrideSettings :: !(Maybe FilePath)
  }
  deriving Show

-- | Represents the basic environment of the app, available immediately
-- on entry into main
data AppEnv = AppEnv
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appCliOptions     :: !CliOptions
  }

-- | Represents the basic configuration available to the app without reading
-- any particular files
data AppBaseConfig = AppBaseConfig
  { appEnv       :: !AppEnv
  , appConfigDir :: !FilePath
  }

-- | Represents the complete environment required to run the app
data App = App
  { appBaseConfig  :: !AppBaseConfig
  , appSettingsRef :: !(SomeRef Settings)
  }

-- | HasAppEnv constrains any env that has the basic environment variables
class HasAppEnv env where
  appEnvL :: Lens' env AppEnv

instance HasAppEnv AppEnv where
  appEnvL = id

instance HasAppEnv AppBaseConfig where
  appEnvL = lens appEnv (\x y -> x { appEnv = y })

instance HasAppEnv App where
  appEnvL = appBaseConfigL . appEnvL

-- | HasCliOptions constrains any env that contains the CLI options described
-- by the app
class HasAppCliOptions env where
  appCliOptionsL :: Lens' env CliOptions

instance HasAppCliOptions CliOptions where
  appCliOptionsL = id

instance HasAppCliOptions AppEnv where
  appCliOptionsL = lens appCliOptions (\x y -> x { appCliOptions = y })

instance HasAppCliOptions AppBaseConfig where
  appCliOptionsL = appEnvL . appCliOptionsL

instance HasAppCliOptions App where
  appCliOptionsL = appEnvL . appCliOptionsL

-- HasAppConfigDir
class HasAppConfigDir env where
  appConfigDirL :: Lens' env FilePath

instance HasAppConfigDir FilePath where
  appConfigDirL = id

instance HasAppConfigDir AppBaseConfig where
  appConfigDirL = lens appConfigDir (\x y -> x { appConfigDir = y })

instance HasAppConfigDir App where
  appConfigDirL = appBaseConfigL . appConfigDirL

-- HasSettingsFileName
class HasOptionsOverrideSettings env where
  optionsOverrideSettingsL :: Lens' env (Maybe FilePath)

instance HasOptionsOverrideSettings (Maybe FilePath) where
  optionsOverrideSettingsL = id

instance HasOptionsOverrideSettings CliOptions where
  optionsOverrideSettingsL =
    lens optionsOverrideSettings (\x y -> x { optionsOverrideSettings = y })

instance HasOptionsOverrideSettings AppBaseConfig where
  optionsOverrideSettingsL = appCliOptionsL . optionsOverrideSettingsL

instance HasOptionsOverrideSettings App where
  optionsOverrideSettingsL = appBaseConfigL . optionsOverrideSettingsL

-- HasAppBaseConfig
class HasAppBaseConfig env where
  appBaseConfigL :: Lens' env AppBaseConfig

instance HasAppBaseConfig AppBaseConfig where
  appBaseConfigL = id

instance HasAppBaseConfig App where
  appBaseConfigL = lens appBaseConfig (\x y -> x { appBaseConfig = y })

-- HasAppSettings
class HasAppSettingsRef env where
  appSettingsRefL :: Lens' env (SomeRef Settings)

instance HasAppSettingsRef (SomeRef Settings) where
  appSettingsRefL = id

instance HasAppSettingsRef App where
  appSettingsRefL = lens appSettingsRef (\x y -> x { appSettingsRef = y })

-- HasLogFunc
instance HasLogFunc AppEnv where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasLogFunc AppBaseConfig where
  logFuncL = appEnvL . logFuncL

instance HasLogFunc App where
  logFuncL = appBaseConfigL . logFuncL

-- HasProcessContext
instance HasProcessContext AppEnv where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

instance HasProcessContext AppBaseConfig where
  processContextL =
    lens (appProcessContext . appEnv) (flip (set processContextL))

instance HasProcessContext App where
  processContextL = appBaseConfigL . processContextL
