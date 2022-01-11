module PlatformRunner.Settings
  ( -- * Types
    Internal.Settings(..)
  , Internal.Difficulty(..)
  , Internal.DisplayMode(..)

    -- * Defaults
  , Internal.defaultSettings
  , defaultWindowTitle
  , defaultViewDimensions

    -- * IO helpers
  , getOrCreateAppConfigDir
  , readOrCreateAppSettings
  , writeSettingsSync
  , writeSettingsAsync

    -- * State helpers
  , getSettings
  , putSettings
  , modifySettings

    -- * Settings-related methods
  , getGlossDisplayMode
  , getGameDimensions
  , getPlatformRunnerParameters
  ) where

import qualified Apecs.Gloss                   as Gloss
                                                ( Display(..) )
import           PlatformRunner.Env
import           PlatformRunner.Settings.Internal
                                               as Internal
import           RIO
import           RIO.State


import           Data.Bitraversable             ( bimapM )
import           Data.Yaml               hiding ( ParseException )
import qualified Data.Yaml                     as Yaml
                                                ( ParseException )
import           Linear                         ( V2(V2) )
import           PlatformRunner.Game.Constant   ( PlatformRunnerConstants(..)
                                                , platformRunnerConstants
                                                )
-- import           PlatformRunner.Game.Constant   ( PlatformRunnerConstants )
import           PlatformRunner.Types           ( Dimensions(Dimensions) )
import           RIO.Directory                  ( XdgDirectory(XdgConfig)
                                                , createDirectoryIfMissing
                                                , doesFileExist
                                                , getXdgDirectory
                                                , makeAbsolute
                                                )
import           RIO.FilePath                   ( (</>) )
import           YamlParse.Applicative          ( prettySchema )


--------------------------------------------------------------------------------
defaultWindowTitle :: IsString s => s
defaultWindowTitle = "Platform Runner"

-- | Expected view dimensions in meters. Should be 16:9
defaultViewDimensions :: V2 Double
defaultViewDimensions = V2 48 27

--------------------------------------------------------------------------------
data SettingsFileReadException
  = ParseException FilePath Yaml.ParseException
  | ReadException FilePath IOException
  deriving Show

instance Exception SettingsFileReadException


--------------------------------------------------------------------------------
-- | Returns the filepath to the global settings file or the override path 
-- provided
getAppSettingsPath
  :: ( HasConfigElem env ConfigDir
     , HasConfigElem env OptionOverrideSettingsPath
     , MonadIO m
     , MonadReader env m
     )
  => Maybe String
  -> m FilePath
getAppSettingsPath extTag = do
  ConfigDir                  configDir        <- viewConfig
  OptionOverrideSettingsPath overrideFileName <- viewConfig

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
  xdgConfigDir <- getXdgDirectory XdgConfig "" <&> (</> defaultConfigFolderName)
  createDirectoryIfMissing False xdgConfigDir
  return xdgConfigDir


--------------------------------------------------------------------------------
-- | Writes the provided settings to the file specified by the current
-- environment. Intended for initialization.
writeSettingsSync'
  :: ( HasConfigElem env OptionOverrideSettingsPath
     , HasConfigElem env ConfigDir
     , MonadIO m
     , MonadReader env m
     )
  => Settings
  -> m ()
writeSettingsSync' settings = do
  filePath <- getAppSettingsPath Nothing
  liftIO $ encodeFile filePath settings

-- | Writes the settings currently in the environment to the file specified by 
-- the current environment. NOT thread-safe, will need STM.
writeSettingsSync
  :: ( HasConfigElem env OptionOverrideSettingsPath
     , HasConfigElem env ConfigDir
     , MonadIO m
     , MonadState Settings m
     , MonadReader env m
     )
  => m ()
writeSettingsSync = do
  filePath <- getAppSettingsPath Nothing
  settings <- get
  liftIO $ encodeFile filePath settings

writeSettingsAsync
  :: ( HasConfigElem env OptionOverrideSettingsPath
     , HasConfigElem env ConfigDir
     , MonadIO m
     , MonadState Settings m
     , MonadReader env m
     )
  => m ()
writeSettingsAsync = do
  filePath <- getAppSettingsPath Nothing
  settings <- get
  liftIO $ withAsync (encodeFile filePath settings) $ \_ -> pure ()

writeSettingsDoc
  :: ( HasConfigElem env OptionOverrideSettingsPath
     , HasConfigElem env ConfigDir
     , MonadIO m
     , MonadReader env m
     )
  => m ()
writeSettingsDoc = do
  filePath <- getAppSettingsPath (Just defaultSettingsDocExt)
  writeFileUtf8 filePath $ prettySchema settingsSchema


--------------------------------------------------------------------------------
-- | Reads settings from the file specified by the environment.
readSettings
  :: ( HasLogFunc env
     , HasConfigElem env OptionOverrideSettingsPath
     , HasConfigElem env ConfigDir
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


--------------------------------------------------------------------------------
-- | Read the settings file specified by the environment, or create it if no
-- such file can be found.
readOrCreateAppSettings
  :: ( HasConfigElem env ConfigDir
     , HasConfigElem env OptionOverrideSettingsPath
     , HasLogFunc env
     , MonadUnliftIO m
     , MonadReader env m
     )
  => m (Either SettingsFileReadException Settings)
readOrCreateAppSettings = do
  settingsFilePath   <- getAppSettingsPath Nothing
  settingsFileExists <- doesFileExist settingsFilePath
  unless settingsFileExists $ writeSettingsSync' defaultSettings

  -- TODO move into initialization function
  settingsDocFilePath   <- getAppSettingsPath (Just defaultSettingsDocExt)
  settingsDocFileExists <- doesFileExist settingsDocFilePath
  unless settingsDocFileExists writeSettingsDoc

  readSettings

--------------------------------------------------------------------------------
-- | Convenience wrapper for State.get specialized to Settings
getSettings :: MonadState Settings m => m Settings
getSettings = get

-- | Conveniance wrapper for State.write specialized to Settings. Also triggers
-- an async write to the settings file.
putSettings
  :: ( MonadState Settings m
     , HasConfigElem env OptionOverrideSettingsPath
     , HasConfigElem env ConfigDir
     , MonadIO m
     , MonadReader env m
     )
  => Settings
  -> m ()
putSettings settings = do
  put settings
  writeSettingsAsync

-- | Convenience wrapping for State.modify' specialized to Settings. Also
-- triggers an async write to the settigns file.
modifySettings
  :: ( MonadState Settings m
     , HasConfigElem env OptionOverrideSettingsPath
     , HasConfigElem env ConfigDir
     , MonadIO m
     , MonadReader env m
     )
  => (Settings -> Settings)
  -> m ()
modifySettings f = do
  modify' f
  writeSettingsAsync


--------------------------------------------------------------------------------
-- | Gets an appropriate gloss display mode based on our current settings.
getGlossDisplayMode :: (MonadState Settings m) => m Gloss.Display
getGlossDisplayMode = do
  displayMode <- view displayModeL <$> getSettings

  return $ case displayMode of
    Fullscreen -> Gloss.FullScreen
    Windowed (Dimensions (V2 width height)) ->
      Gloss.InWindow defaultWindowTitle (width, height) (10, 10)

getGameDimensions
  :: (HasConfigElem env ScreenSize, MonadState Settings m, MonadReader env m)
  => m Dimensions
getGameDimensions = do
  ScreenSize screenSize <- viewConfig
  displayMode           <- view displayModeL <$> getSettings

  return $ case displayMode of
    Fullscreen    -> screenSize
    Windowed dims -> dims

getPlatformRunnerParameters
  :: (HasConfigElem env ScreenSize, MonadReader env m, MonadState Settings m)
  => m PlatformRunnerConstants
getPlatformRunnerParameters = do
  difficulty <- view difficultyL <$> get
  let parameters = platformRunnerConstants difficulty

  Dimensions gameDims <- getGameDimensions

  let V2 scaleX _ = (fromIntegral <$> gameDims) / defaultViewDimensions

  return $ parameters { cameraScaleFactor = scaleX }

