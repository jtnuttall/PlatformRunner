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
import           PlatformRunner.AppEnv
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
  :: (HasAppConfigDir env, HasOptionsOverrideSettings env) => RIO env FilePath
getAppSettingsPath = do
  configDir        <- view appConfigDirL
  overrideFileName <- view optionsOverrideSettingsL

  case overrideFileName of
    Just fileName -> makeAbsolute fileName
    Nothing       -> return $ configDir </> defaultSettingsFileName

getOrCreateAppConfigDir :: RIO env FilePath
getOrCreateAppConfigDir = do
  xdgConfigDir <- getXdgDirectory XdgConfig "" <&> (</> configFolderName)
  createDirectoryIfMissing False xdgConfigDir
  return xdgConfigDir

writeSettings
  :: (HasOptionsOverrideSettings env, HasAppConfigDir env)
  => Settings
  -> RIO env ()
writeSettings settings = do
  filePath <- getAppSettingsPath
  liftIO $ encodeFile filePath settings

readSettings
  :: (HasLogFunc env, HasOptionsOverrideSettings env, HasAppConfigDir env)
  => RIO env (Either ReadException Settings)
readSettings = do
  filePath <- getAppSettingsPath
  exists   <- doesFileExist filePath
  if exists
    then do
      contents <- liftIO $ decodeFileWithWarnings filePath

      bitraverse
        (return . ParseException)
        (\(warnings, settings) ->
          mapM_ (logWarn . ("Yaml warning: " <>) . displayShow) warnings
            >> return settings
        )
        contents
    else return $ Left FileDoesNotExist

readOrCreateAppSettings
  :: (HasAppConfigDir env, HasOptionsOverrideSettings env, HasLogFunc env)
  => RIO env (Either ReadException Settings)
readOrCreateAppSettings = do
  filePath   <- getAppSettingsPath
  fileExists <- doesFileExist filePath
  unless fileExists $ writeSettings defaultSettings
  readSettings
