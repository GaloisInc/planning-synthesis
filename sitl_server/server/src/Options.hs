{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Options where

import           Protocol (Position,Heading,readHome,portland,north,Wind,noWind
                          ,showHome)

import           Config
import           Control.Monad ((>=>),unless)
import           Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network (PortNumber)
import           Numeric (readFloat)
import           System.Console.GetOpt
                     (getOpt,ArgOrder(..),ArgDescr(..),OptDescr(..),usageInfo)
import           System.Directory (makeAbsolute,doesFileExist)
import           System.Environment (getArgs,getProgName)
import           System.Exit (exitSuccess,exitFailure)
import           System.FilePath ((</>),takeDirectory,isAbsolute)


-- Option Parsing --------------------------------------------------------------

data Config = Config { cfgPort          :: !PortNumber
                     , cfgBuildPath     :: FilePath
                     , cfgArduPilotPath :: FilePath
                     , cfgWind          :: !Wind
                     , cfgTest          :: Bool
                     , cfgDrop          :: Maybe Double
                     , cfgSimConfig     :: !SimConfig
                     } deriving (Show)

type Home = (Position,Heading)

data SimConfig = SimConfig { scHome :: !Home
                             -- ^ The home position for the controlled instance
                           , scObstacles :: [ObstacleDef]
                             -- ^ Obstacle definitions
                           } deriving (Show)

data ObstacleDef = ObstacleDef { oHome   :: !Home
                               , oScript :: FilePath
                               , oName   :: String
                               } deriving (Show)

-- | Fix paths to be absolute in the config structure.
makePathsAbsolute :: Config -> IO Config
makePathsAbsolute cfg =
  do buildPath'     <- makeAbsolute (cfgBuildPath     cfg)
     arduPilotPath' <- makeAbsolute (cfgArduPilotPath cfg)
     return cfg { cfgBuildPath     = buildPath'
                , cfgArduPilotPath = arduPilotPath'
                }

newtype Parser a = Parser { runParser :: a -> IO a }

instance Monoid (Parser a) where
  mempty                        = Parser return
  mappend (Parser f) (Parser g) = Parser (f >=> g)

defaultConfig :: Config
defaultConfig  = Config { cfgPort          = 31320
                        , cfgBuildPath     = "build"
                        , cfgArduPilotPath = "deps" </> "ardupilot"
                        , cfgWind          = noWind
                        , cfgTest          = False
                        , cfgDrop          = Nothing
                        , cfgSimConfig     = defaultSimConfig
                        }

defaultSimConfig :: SimConfig
defaultSimConfig  = SimConfig { scHome      = (portland,north)
                              , scObstacles = []
                              }

options :: [OptDescr (Parser Config)]
options  =
  [ Option "h" ["help"] (NoArg showHelp)
    "display this message"

  , Option "b" ["build-path"] (ReqArg setBuildPath "BUILDDIR")
    "location of build artifacts"

  , Option "a" ["ardupilot-path"] (ReqArg setArduPilotPath "PATH")
    "location of the ardupilot submodule"

  , Option "p" ["port"] (ReqArg setPort "PORT")
    "set the port to listen on"

  , Option "d" ["drop"] (ReqArg setDrop "[0,1]")
    "the chance to drop an obstacle's position packet"

  , Option "t" ["test"] (NoArg setTest)
    "test the SITL interaction"
  ]

showUsage :: [String] -> IO ()
showUsage errs =
  do prog <- getProgName
     let header = "Usage: " ++ prog ++ " [OPTIONS]"
     putStrLn (usageInfo (unlines (errs ++ [header])) options)

showHelp :: Parser a
showHelp  = Parser $ \ _ ->
  do showUsage []
     exitSuccess

setBuildPath :: String -> Parser Config
setBuildPath path = Parser $ \ cfg -> return cfg { cfgBuildPath = path }

setArduPilotPath :: String -> Parser Config
setArduPilotPath path = Parser $ \ cfg -> return cfg { cfgArduPilotPath = path }

setPort :: String -> Parser Config
setPort port = Parser $ \ cfg ->
     case reads port of
       [(x,[])] ->    return cfg { cfgPort = fromIntegral (x :: Int) }
       _        -> do showUsage ["Invalid port"]
                      exitFailure

setTest :: Parser Config
setTest  = Parser (\ cfg -> return cfg { cfgTest = True })

setDrop :: String -> Parser Config
setDrop str = Parser $ \ cfg ->
  case readFloat str of
    [(d,_)] | 0 <= d && d <= 1 -> return cfg { cfgDrop = Just d }
            | otherwise        -> err "Chance to drop must be between 0 and 1"
    _                          -> err "Unable to parse `drop` value"
  where
  err msg =
    do showUsage [msg]
       exitFailure

setSimConfig :: FilePath -> Parser Config
setSimConfig path = Parser $ \ cfg ->
  do sc <- readSimConfig path
     return cfg { cfgSimConfig = sc }


getConfig :: IO Config
getConfig  =
  do args <- getArgs
     case getOpt (ReturnInOrder setSimConfig) options args of
       (ps,_,[])  -> makePathsAbsolute =<< runParser (mconcat ps) defaultConfig
       (_,_,errs) -> do showUsage errs
                        exitFailure


-- Locations File Generation ---------------------------------------------------

genLocationsFile :: SimConfig -> String
genLocationsFile SimConfig { .. } = unlines (controlledLoc : obstacleLocs)
  where
  -- given a name and home, dump out a named location for locations.txt
  locLine n h = showString n
              $ showChar '='
              $ showHome h ""

  controlledLoc = locLine "controlled" scHome
  obstacleLocs  = [ locLine name oHome | ObstacleDef { .. } <- scObstacles
                                       , let name = "obstacle-" ++ oName ]

-- SimConfig Parsing -----------------------------------------------------------

readSimConfig :: FilePath -> IO SimConfig
readSimConfig path =
  do bytes <- T.readFile path
     case parse bytes of
       Right cfg -> do cfgBase <- makeAbsolute (takeDirectory path)
                       fixObstacles cfgBase =<< parseSimConfig cfg
       Left err  -> do showUsage [err]
                       exitFailure

-- | Fix the obstacle script paths: if the paths for the obstacle scripts aren't
-- absolute already, they are assumed to be relative to the config file.
fixObstacles :: FilePath -> SimConfig -> IO SimConfig
fixObstacles base SimConfig { .. } =
  do os' <- mapM fixDef scObstacles
     return SimConfig { scObstacles = os', .. }
  where
  fixDef o @ ObstacleDef { .. }
    | isAbsolute oScript =
      do requireScript oScript
         return o

    | otherwise          =
      do let script = base </> oScript
         requireScript script
         return ObstacleDef { oScript = script, .. }

  requireScript path =
    do b <- doesFileExist path
       unless b $ do showUsage ["obstacle script missing: " ++ path]
                     exitFailure

parseError :: String -> IO a
parseError msg =
  do showUsage ["Sim config parse error: " ++ msg]
     exitFailure

parseSimConfig :: Value -> IO SimConfig
parseSimConfig val =
  do fs <- mapM parseSection =<< valSections val
     return $! foldl (flip id) defaultSimConfig fs

parseSection :: Section -> IO (SimConfig -> SimConfig)
parseSection Section { .. } =
  case sectionName of
    "controlled" -> parseControlled sectionValue
    "obstacles"  -> parseObstacles  sectionValue
    _            -> parseError ("invalid section: " ++ T.unpack sectionName)

parseControlled :: Value -> IO (SimConfig -> SimConfig)
parseControlled val =
  do xs <- valSections val
     case xs of
       [Section "home" txt] -> do h <- valHome txt
                                  return (\ sc -> sc { scHome = h })
       _ -> parseError "couldn't parse controlled section"

parseObstacles :: Value -> IO (SimConfig -> SimConfig)
parseObstacles val =
  do os <- mapM parseObstacleDef =<< valList val
     return (\ sc -> sc { scObstacles = os })

parseObstacleDef :: Value -> IO ObstacleDef
parseObstacleDef val =
  do xs      <- valSections val
     oHome   <- valHome =<< section "home"   xs
     oScript <- valText =<< section "script" xs
     oName   <- valText =<< section "name"   xs
     return ObstacleDef { .. }

valSections :: Value -> IO [Section]
valSections (Sections xs) = return xs
valSections _             = parseError "expected sections"

valList :: Value -> IO [Value]
valList (List xs) = return xs
valList _         = parseError "expected a list"

valText :: Value -> IO String
valText (Text txt) = return (T.unpack txt)
valText _          = parseError "expected text"

valHome :: Value -> IO Home
valHome val =
  do txt <- valText val
     case readHome txt of
       [(h,[])] -> return h
       _        -> parseError "failed parsing home value"

section :: String -> [Section] -> IO Value
section label xs =
  case find (\s -> sectionName s == l') xs of
    Just Section { .. } -> return sectionValue
    Nothing             -> parseError ("Missing section: " ++ label)
  where
  l' = T.pack label
