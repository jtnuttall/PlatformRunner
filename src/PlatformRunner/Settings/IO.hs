module PlatformRunner.Settings.IO
  ( readSettings
  , writeSettings
  , writeSettingsDoc
  , getOrCreateAppConfigDir
  , readOrCreateAppSettings
  ) where

import           Data.Bitraversable             ( bimapM )
import           Data.Yaml               hiding ( ParseException )
import qualified Data.Yaml                     as Yaml
                                                ( ParseException )
import           GHC.IO.Exception               ( IOErrorType(..)
                                                , IOException(IOError)
                                                )
import           PlatformRunner.Env
import           PlatformRunner.Import
import           PlatformRunner.Settings.Defaults
import           PlatformRunner.Settings.Types  ( Settings
                                                , settingsSchema
                                                )
import           RIO.Directory                  ( XdgDirectory(XdgConfig)
                                                , createDirectoryIfMissing
                                                , doesFileExist
                                                , getXdgDirectory
                                                , makeAbsolute
                                                )
import           RIO.FilePath                   ( (</>) )
import           YamlParse.Applicative          ( prettySchema )

data SettingsFileReadException
  = ParseException FilePath Yaml.ParseException
  | ReadException FilePath IOException
  deriving Show

instance Exception SettingsFileReadException

-- | Returns the filepath to the global settings file or the override path provided
getAppSettingsPath
  :: ( HasAppConfigDir env
     , HasOptionsOverrideSettings env
     , MonadIO m
     , MonadReader env m
     )
  => Maybe String
  -> m FilePath
getAppSettingsPath extTag = do
  configDir        <- view appConfigDirL
  overrideFileName <- view optionsOverrideSettingsL

  case overrideFileName of
    Just fileName -> makeAbsolute fileName
    Nothing ->
      return
        $   configDir
        </> defaultSettingsBaseName
        <>  fromMaybe "" extTag
        <>  defaultSettingsExt

-- | Gets or creates the App's config directory. Currently this will be the XDG 
-- config directory.
-- On *nix, this should be $HOME/.config
-- On Windows, this should usually be C:\\Users\\$username
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
  filePath <- getAppSettingsPath Nothing
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
  filePath <- getAppSettingsPath Nothing
  settings <- readSomeRef =<< view appSettingsRefL
  liftIO $ encodeFile filePath settings

writeSettingsDoc
  :: ( HasOptionsOverrideSettings env
     , HasAppConfigDir env
     , MonadIO m
     , MonadReader env m
     )
  => m ()
writeSettingsDoc = do
  filePath <- getAppSettingsPath (Just defaultSettingsDocExt)
  writeFileUtf8 filePath $ prettySchema settingsSchema

-- | Reads settings from the file specified by the environment.
readSettings
  :: ( HasLogFunc env
     , HasOptionsOverrideSettings env
     , HasAppConfigDir env
     , MonadUnliftIO m
     , MonadReader env m
     )
  => m (Either SettingsFileReadException Settings)
readSettings = do
  filePath       <- getAppSettingsPath Nothing
  fileReadResult <- try $ liftIO $ decodeFileWithWarnings filePath

  case fileReadResult of
    Left ioe -> do
      logError
        $  "Encountered error reading "
        <> displayShow filePath
        <> ": "
        <> displayShow ioe
      return $ Left (ReadException filePath ioe)

    Right parseResult -> bimapM
      (return . ParseException filePath)
      (\(warnings, settings) ->
        mapM_ (logWarn . ("Yaml warning: " <>) . displayShow) warnings
          >> return settings
      )
      parseResult

-- | Read the settings file specified by the environment, or create it if no such
-- file can be found.
readOrCreateAppSettings
  :: ( HasAppConfigDir env
     , HasOptionsOverrideSettings env
     , HasLogFunc env
     , MonadUnliftIO m
     , MonadReader env m
     )
  => m (Either SettingsFileReadException Settings)
readOrCreateAppSettings = do
  settingsFilePath   <- getAppSettingsPath Nothing
  settingsFileExists <- doesFileExist settingsFilePath
  unless settingsFileExists $ writeSettings' defaultSettings

  -- TODO move into initialization function
  settingsDocFilePath   <- getAppSettingsPath (Just defaultSettingsDocExt)
  settingsDocFileExists <- doesFileExist settingsDocFilePath
  unless settingsDocFileExists writeSettingsDoc

  readSettings

-- | Read the settings file specified by the environment, or create it if no such
-- file can be found. If the file is missing expected fields, use `defaultSettings`
-- to populate them.
readOrCreateAppSettingsWithDefault
  :: ( HasAppConfigDir env
     , HasOptionsOverrideSettings env
     , HasLogFunc env
     , MonadUnliftIO m
     , MonadReader env m
     )
  => Settings
  -> m (Either SettingsFileReadException Settings)
readOrCreateAppSettingsWithDefault defaultSettings = do
  readResult <- readOrCreateAppSettings

  case readResult of
    Right settings              -> return readResult
    Left  (ReadException  _ _ ) -> return readResult
    Left  (ParseException _ pe) -> case pe of
      NonScalarKey                -> undefined
      UnknownAlias s              -> undefined
      UnexpectedEvent m_ev ma     -> undefined
      InvalidYaml m_ye            -> undefined
      MultipleDocuments           -> undefined
      AesonException      s       -> undefined
      OtherParseException se      -> undefined
      NonStringKey        jpes    -> undefined
      NonStringKeyAlias s va      -> undefined
      CyclicIncludes              -> undefined
      LoadSettingsException s pe' -> undefined
