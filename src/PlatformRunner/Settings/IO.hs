module PlatformRunner.Settings.IO
  ( readSettings
  , writeSettings
  , getOrCreateAppConfigDir
  , readOrCreateAppSettings
  ) where

import           Data.Yaml                      ( ParseException
                                                , decodeFileWithWarnings
                                                , encodeFile
                                                )
import           PlatformRunner.Env
import           PlatformRunner.Import
import           PlatformRunner.Settings.Defaults
import           PlatformRunner.Settings.Types  ( Settings )
import           RIO.Directory                  ( XdgDirectory(XdgConfig)
                                                , createDirectoryIfMissing
                                                , doesFileExist
                                                , getXdgDirectory
                                                , makeAbsolute
                                                )
import           RIO.FilePath                   ( (</>) )

data ReadException = ParseException ParseException | FileDoesNotExist deriving (Show)

instance Exception ReadException

-- | Returns the filepath to the global settings file or the override path provided
getAppSettingsPath
  :: ( HasAppConfigDir env
     , HasOptionsOverrideSettings env
     , MonadIO m
     , MonadReader env m
     )
  => m FilePath
getAppSettingsPath = do
  configDir        <- view appConfigDirL
  overrideFileName <- view optionsOverrideSettingsL

  case overrideFileName of
    Just fileName -> makeAbsolute fileName
    Nothing       -> return $ configDir </> defaultSettingsFileName

-- | Gets or creates the App's config directory. Currently this will be the XDG 
-- config directory
getOrCreateAppConfigDir :: (MonadIO m) => m FilePath
getOrCreateAppConfigDir = do
  xdgConfigDir <- getXdgDirectory XdgConfig "" <&> (</> configFolderName)
  createDirectoryIfMissing False xdgConfigDir
  return xdgConfigDir

-- | Writes the provided settings to the file specified by the current environment. 
-- Intended for initialization.
writeSettings'
  :: ( HasOptionsOverrideSettings env
     , HasAppConfigDir env
     , MonadIO m
     , MonadReader env m
     )
  => Settings
  -> m ()
writeSettings' settings = do
  filePath <- getAppSettingsPath
  liftIO $ encodeFile filePath settings

-- | Writes the settings currently in the environment to the file specified by 
-- the current environment. NOT thread-safe, will need STM.
writeSettings
  :: ( HasOptionsOverrideSettings env
     , HasAppConfigDir env
     , HasAppSettingsRef env
     , MonadIO m
     , MonadReader env m
     )
  => m ()
writeSettings = do
  filePath <- getAppSettingsPath
  settings <- readSomeRef =<< view appSettingsRefL
  liftIO $ encodeFile filePath settings

-- | Reads settings from the file specified by the environment.
readSettings
  :: ( HasLogFunc env
     , HasOptionsOverrideSettings env
     , HasAppConfigDir env
     , MonadIO m
     , MonadReader env m
     )
  => m (Either ReadException Settings)
readSettings = do
  filePath   <- getAppSettingsPath
  fileExists <- doesFileExist filePath
  if not fileExists
    then return $ Left FileDoesNotExist
    else
      bitraverse
          (return . ParseException)
          (\(warnings, settings) ->
            mapM_ (logWarn . ("Yaml warning: " <>) . displayShow) warnings
              >> return settings
          )
        =<< liftIO (decodeFileWithWarnings filePath)

-- | Read the settings file specified by the environment, or create it if no such
-- file can be found.
readOrCreateAppSettings
  :: ( HasAppConfigDir env
     , HasOptionsOverrideSettings env
     , HasLogFunc env
     , MonadIO m
     , MonadReader env m
     )
  => m (Either ReadException Settings)
readOrCreateAppSettings = do
  filePath   <- getAppSettingsPath
  fileExists <- doesFileExist filePath
  unless fileExists $ writeSettings' defaultSettings
  readSettings
