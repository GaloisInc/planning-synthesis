{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ParallelListComp #-}

module SITL (
  -- * Server Interface
  withSitlServer, ServerHandle(),

  -- * SITL API
  getObstacles,
  create, arm, isArmed, disarm, getPosition, getPositions,

  -- ** Controlled API
  moveTo,

  -- ** Obstacle API
  loadWaypoints, followWaypoints
  ) where

import           Options (Config(..),SimConfig(..),ObstacleDef(..))
import           Protocol
                     (Instance(..),instanceName,instanceId,Position(..),Heading
                     ,showPosition)

import           Control.Concurrent (ThreadId,forkIO,killThread)
import           Control.Concurrent.Async (Async,async,wait)
import           Control.Concurrent.STM
                     (atomically,TChan,newTChanIO,writeTChan,readTChan
                     ,TMVar,newTMVarIO,newEmptyTMVarIO,putTMVar,takeTMVar
                     ,readTVar,readTMVar,registerDelay,orElse,retry,TVar
                     ,writeTVar,newTVarIO)
import qualified Control.Exception as X
import           Control.Monad (forever,unless)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Typeable (Typeable)
import           Network (listenOn,PortID(..),accept,sClose)
import           System.FilePath ((</>))
import           System.IO
                     (Handle,hPutStrLn,hGetLine,hPutStrLn,hClose,hSetBuffering
                     ,BufferMode(..))
import           System.Process (callCommand)
import           System.Posix.Signals (signalProcess,softwareTermination)
import           System.Posix.Types (ProcessID)


-- SITL Server -----------------------------------------------------------------

data ServerHandle = ServerHandle { shConfig    :: !Config
                                 , shInstances :: !(TMVar (Map.Map Instance SITL))
                                 }

lookupInst :: Instance -> ServerHandle -> IO (Maybe SITL)
lookupInst inst ServerHandle { .. } =
  do insts <- atomically (readTMVar shInstances)
     return (Map.lookup inst insts)


data SITLException = NoSITLInstance Instance
                     deriving (Typeable,Show)

instance X.Exception SITLException


-- | Get a SITL instance from the server state, failing if it doesn't exist.
getInst :: Instance -> ServerHandle -> IO SITL
getInst inst sh =
  do mb <- lookupInst inst sh
     case mb of
       Just sitl -> return sitl
       Nothing   -> X.throwIO (NoSITLInstance inst)


data TimeoutError = OperationTimedOut
                    deriving (Show,Typeable)

instance X.Exception TimeoutError

blockOn :: (TMVar a -> IO ()) -> IO a
blockOn  = blockOn' Nothing

blockOn' :: Maybe Int -> (TMVar a -> IO ()) -> IO a
blockOn' mbTimeout k =
  do result <- newEmptyTMVarIO
     k result
     case mbTimeout of
       Nothing -> atomically (takeTMVar result)
       Just t  -> do mb <- takeTMVarTimeout result t
                     case mb of
                       Just a  -> return a
                       Nothing -> X.throwIO OperationTimedOut

unblock :: a -> TMVar a -> IO ()
unblock a var = atomically (putTMVar var a)

withSitlServer :: Config -> (ServerHandle -> IO a) -> IO a
withSitlServer cfg = X.bracket (sitlServer cfg) shutdownServer

-- | Start the sitl server on port 9001, and make sure that a controlled
-- instance of the SITL is running.
sitlServer :: Config -> IO ServerHandle
sitlServer shConfig =
  do shInstances <- newTMVarIO Map.empty

     let sh = ServerHandle { .. }
         SimConfig { .. } = cfgSimConfig shConfig

     -- create all of the obstacles
     let insts = [ Obstacle oName inst | ObstacleDef { .. } <- scObstacles
                                       | inst               <- [0 .. ] ]

     -- create the controlled instance, and the obstacles
     putStrLn "Creating instances"
     running <- mapM wait =<< mapM (`create` sh) (Controlled : insts)
     unless (and running) (fail "Not all instances started")

     -- load all waypoint scripts into the obstacles 
     unless (null insts) (putStrLn "Loading waypoint scripts")
     mapM_ wait =<< sequence [ loadWaypoints inst oScript sh
                             | ObstacleDef { .. } <- scObstacles
                             | inst               <- insts ]

     -- arm everything
     putStrLn "Arming instances"
     allArmed <- mapM wait =<< mapM (`arm` sh) (Controlled : insts)

     unless (and allArmed)
            (fail "Not all instances armed successfully")

     return sh

shutdownServer :: ServerHandle -> IO ()
shutdownServer ServerHandle { .. } =
  do putStrLn "Killing server..."
     mapM_ killSITL =<< atomically (readTMVar shInstances)

-- | Create an instance of the SITL, and register it with the server.
create :: Instance -> ServerHandle -> IO (Async Bool)
create inst ServerHandle { .. } =
  do created <- atomically $
         do insts <- readTMVar shInstances
            return (Map.member inst insts)

     async $
       if created
          then return False
          else do mb <- forkSITL shConfig inst
                  case mb of
                    Just sitl -> atomically $
                      do insts <- takeTMVar shInstances
                         putTMVar shInstances $! Map.insert inst sitl insts
                         return True

                    Nothing ->
                         return False

-- | Return the instance names of all the obstacle SITL instances.
getObstacles :: ServerHandle -> IO [Instance]
getObstacles ServerHandle { .. } =
  do insts <- atomically (readTMVar shInstances)
     return [ o | o @ Obstacle{} <- Map.keys insts ]

-- | Arm the instance provided.
arm :: Instance -> ServerHandle -> IO (Async Bool)
arm inst sh =
  do sitl <- getInst inst sh
     async (blockOn (writeSITL sitl . SITLArm (inst == Controlled)))

isArmed :: Instance -> ServerHandle -> IO Bool
isArmed inst sh =
  do sitl <- getInst inst sh
     atomically (readTVar (sitlArmed sitl))

disarm :: Instance -> ServerHandle -> IO (Async ())
disarm inst sh =
  do sitl <- getInst inst sh
     async (blockOn (writeSITL sitl . SITLDisarm))

getPosition :: Instance -> ServerHandle -> IO (Position,Heading)
getPosition inst sh =
  do sitl <- getInst inst sh
     atomically (readTVar (sitlPosition sitl))

getPositions :: ServerHandle -> IO [(Instance,Position,Heading)]
getPositions sh =
  do insts <- atomically (readTMVar (shInstances sh))
     mapM readPosition (Map.toList insts)
  where
  readPosition (i,sitl) =
    do (p,h) <- atomically (readTVar (sitlPosition sitl))
       return (i,p,h)

moveTo :: Instance -> Position -> ServerHandle -> IO (Async ())
moveTo inst pos sh =
  do sitl <- getInst inst sh
     async (blockOn (writeSITL sitl . SITLMove pos))

-- | Load waypoints out of a file, into the SITL instance.
loadWaypoints :: Instance -> FilePath -> ServerHandle -> IO (Async ())
loadWaypoints inst path sh =
  do sitl <- getInst inst sh
     async (blockOn (writeSITL sitl . SITLWaypoints path))

-- | Instruct the autopilot to start flying the waypoints given.
followWaypoints :: Instance -> ServerHandle -> IO (Async ())
followWaypoints inst sh =
  do sitl <- getInst inst sh
     async (blockOn (writeSITL sitl . SITLAuto))


-- SITL Management -------------------------------------------------------------

type SITLControl = TChan SITLCommand

data SITLCommand = SITLArm Bool !(TMVar Bool)
                   -- ^ Arm the quadcopter

                 | SITLDisarm !(TMVar ())
                   -- ^ Disarm the quadcopter

                 | SITLMove !Position !(TMVar ())
                   -- ^ Move to the given position

                 | SITLWaypoints FilePath !(TMVar ())
                   -- ^ Set waypoints

                 | SITLAuto !(TMVar ())
                   -- ^ Enter full auto, and fly waypoints

                 | SITLResponse Response

data SITL = SITL { sitlCommand  :: !SITLControl
                 , sitlHandler  :: !ThreadId
                 , sitlIncoming :: !ThreadId
                 , sitlArmed    :: !(TVar Bool)
                 , sitlPosition :: !(TVar (Position,Heading))
                 , sitlPID      :: !(TVar (Maybe ProcessID))
                 }

writeSITL :: SITL -> SITLCommand -> IO ()
writeSITL SITL { .. } cmd = atomically (writeTChan sitlCommand cmd)

-- | Launch a SITL instance, including the instance number when something other
-- than the commanded instance is being created.
--
-- This forks off two threads:
--
--  1. Translate incoming requests from sitl_proxy into SITLCommands, and place
--     them on the sitlCommand channel
--  2. Handle requests from the sitl server
forkSITL :: Config -> Instance -> IO (Maybe SITL)
forkSITL Config { .. } inst =
  do sitlCommand <- newTChanIO
     done        <- newEmptyTMVarIO
     sitlArmed   <- newTVarIO False
     sitlPosition<- newTVarIO (Position 0 0 0, 0)
     let port = PortNumber (9001 + fromIntegral (instanceId inst))
     sock        <- listenOn port
     sitlPID     <- newTVarIO Nothing
     sitlHandler <- forkIO $ X.bracket_ setup cleanup $
                      do putStrLn ("Listening on port: " ++ show port)
                         (h,_,_)  <- accept sock
                         sClose sock
                         putStrLn ("Client accepted on port: " ++ show port)
                         hSetBuffering h LineBuffering
                         incoming <- forkIO (handleIncoming sitlCommand h)

                         atomically (putTMVar done incoming)

                         handleSITL sitlCommand sitlArmed sitlPosition sitlPID h
                             `X.finally` do putStrLn "Killing off incoming thread"
                                            killThread incoming

     mb <- takeTMVarTimeout done (30 * 1000000)
     case mb of
       Just sitlIncoming ->    return (Just SITL { .. })
       Nothing           -> do killThread sitlHandler
                               putStrLn ("Failed to connect to SITL instance: " ++ instStr)
                               return Nothing

  where

  instStr  = show (instanceId inst)
  instName = instanceName inst

  cmd str =
    do putStrLn (" $ " ++ str)
       callCommand str

  setup = cmd $ unwords
       [ "tmux", "new-window", "-d"
       , "-n", instName
       , "'" ++ "scripts" </> "sitl.sh"
       , "\"" ++ cfgBuildPath ++ "\""
       , "\"" ++ cfgArduPilotPath ++ "\""
       , instStr
       , instName
       , "'"
       ]

  cleanup = return ()


killSITL :: SITL -> IO ()
killSITL SITL { .. } =
  do killThread sitlIncoming
     killThread sitlHandler
     mbPID <- atomically (readTVar sitlPID)
     case mbPID of
       Just pid -> signalProcess softwareTermination pid
       Nothing  -> putStrLn "Unable to kill SITL!"


data Requests = Requests { reqArming    :: [TMVar Bool]
                         , reqDisarming :: [TMVar ()]
                         , reqAck       :: Seq.Seq (TMVar ())
                         }

-- | Handle high-level messages from the sitl instance or server.
handleSITL :: SITLControl -> TVar Bool -> TVar (Position,Heading)
           -> TVar (Maybe ProcessID)
           -> Handle -> IO ()
handleSITL sitl armed pos pid client = go (Requests [] [] Seq.empty)
  where
  go reqs =
    do cmd <- atomically (readTChan sitl)
       case cmd of

         -- request to arm the quadcopter
         SITLArm isControlled req ->
           do hPutStrLn client (showString "Arm " (shows isControlled ""))
              go reqs { reqArming = req : reqArming reqs }

         SITLDisarm req ->
           do hPutStrLn client "Disarm"
              go reqs { reqDisarming = req : reqDisarming reqs }

         SITLMove p req ->
           do hPutStrLn client (showString "Move " (showPosition p ""))
              waitAck req reqs

         SITLWaypoints path req ->
           do hPutStrLn client (showString "Waypoints " path)
              waitAck req reqs

         SITLAuto req ->
           do hPutStrLn client "Auto"
              waitAck req reqs

         -- position update
         SITLResponse (UpdatePosition posLat posLon posAlt hdg) ->
           do atomically (writeTVar pos (Position { .. }, hdg))
              go reqs

         SITLResponse Armed ->
           do mapM_ (unblock True) (reqArming reqs)
              atomically (writeTVar armed True)
              go reqs { reqArming = [] }

         SITLResponse Disarmed ->
           do mapM_ (unblock ()) (reqDisarming reqs)
              atomically (writeTVar armed False)
              go reqs { reqDisarming = [] }

         -- take an ack off of the queue
         SITLResponse Ack ->
           case Seq.viewr (reqAck reqs) of

             rs Seq.:> req ->
               do atomically (putTMVar req ())
                  go reqs { reqAck = rs }

             Seq.EmptyR ->
               do putStrLn "Ack when none was expected"
                  go reqs

         SITLResponse (Pid mb) ->
           do putStrLn ("Connected with PID: " ++ show mb)
              atomically (writeTVar pid mb)
              go reqs


  waitAck req reqs = go reqs { reqAck = req Seq.<| reqAck reqs }

-- | Processes incoming messages from the sitl proxy, placing them on the
-- control queue for the sitl handler.
handleIncoming :: SITLControl -> Handle -> IO ()
handleIncoming sitl h = flip X.finally cleanup $ forever $
  do line <- hGetLine h
     case reads line of
       [(msg,_)] -> atomically (writeTChan sitl (SITLResponse msg))
       _         -> putStrLn ("Failed to parse sitl message: " ++ line)

  where
  cleanup = hClose h




-- Utilities -------------------------------------------------------------------

data Response = UpdatePosition !Double !Double !Double !Double
              | Armed
              | Disarmed
              | Ack
              | Pid (Maybe ProcessID)
                deriving (Read,Show)

-- | Take a TMVar in IO, returning nothing if the timeout (in microseconds)
-- expires.
takeTMVarTimeout :: TMVar a -> Int -> IO (Maybe a)
takeTMVarTimeout var delay =
  do timeout <- registerDelay delay

     let checkTimeout = do expired <- readTVar timeout
                           unless expired retry
                           return Nothing

     atomically ((Just `fmap` takeTMVar var) `orElse` checkTimeout)
