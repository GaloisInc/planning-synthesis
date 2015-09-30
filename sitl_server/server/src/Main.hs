{-# LANGUAGE RecordWildCards #-}

module Main where

import           Options (Config(..),getConfig,genLocationsFile)
import           Protocol
                     (getRequest,putResponse,Request(..),Response(..)
                     ,respondResult,Instance(..),showPosition)
import           SITL
                     (withSitlServer,ServerHandle,isArmed,getObstacles
                     ,getPositions,moveTo,followWaypoints)

import           Control.Concurrent.Async (wait)
import           Control.Monad (forever,zipWithM)
import           Data.IORef (newIORef,readIORef,writeIORef)
import           Network (listenOn,PortID(..),accept)
import           System.FilePath ((</>))
import           System.IO (Handle)
import           System.Random (randomRIO)


-- Server ----------------------------------------------------------------------

main :: IO ()
main  =
  do cfg  <- getConfig

     -- create the locations.txt file
     writeFile (cfgArduPilotPath cfg </> "Tools" </> "autotest" </> "locations.txt")
               (genLocationsFile (cfgSimConfig cfg))

     putStrLn "Starting SITL instances..."
     withSitlServer cfg $ \ server ->
       do insts <- getObstacles server

          sock <- listenOn (PortNumber (cfgPort cfg))
          putStrLn "Waiting for control connection..."
          (h,_,_) <- accept sock
          putStrLn "Controller connected"
          handleClient cfg server insts h


-- | Handle a connection from the controller, proxying all of its commands to
-- the controlled instance.
handleClient :: Config -> ServerHandle -> [Instance] -> Handle -> IO ()
handleClient Config { .. } server obstacles client =
  do positions <- newIORef []
     forever (processMsg positions)

  where
  processMsg oldPs =
    do req <- getRequest client
       case req of

         GetStatus ->
           do putStrLn "Checking status"
              b <- isArmed Controlled server
              putResponse client (respondResult (Status b))

         StartObstacles ->
           do putStrLn "Starting obstacles"
              mapM_ (`followWaypoints` server) obstacles
              putResponse client (respondResult Ack)

         MoveTo loc ->
           do putStrLn $ showString "Moving the controlled instance to: "
                       $ showPosition loc ""
              wait =<< moveTo Controlled loc server
              putResponse client (respondResult Ack)

         GetPositions ->
           do putStrLn "Returning positions"
              ps <- adjustPositions oldPs =<< getPositions server
              putResponse client (respondResult (Positions ps))

  adjustPositions =
    case cfgDrop of
      Just p  -> adjustPositions' p
      Nothing -> \ _ curPs -> return curPs

  adjustPositions' p posCache curPs =
    do prev  <- readIORef posCache
       newPs <- case prev of
                  [] -> return curPs
                  _  -> zipWithM (pickPos p) prev curPs

       writeIORef posCache newPs
       return newPs

  -- As the positions that come from the ServerHandle are in a stable order,
  -- zipping the two lists with a chance to drop is sufficient.  Additionally,
  -- the controlled instance never has its position delayed.
  pickPos _ (Controlled,_,_) pos@(Controlled,_,_) = return pos
  pickPos p old              new                  =
    do s <- randomRIO (0,1)
       if s >= p
          then return new
          else return old
